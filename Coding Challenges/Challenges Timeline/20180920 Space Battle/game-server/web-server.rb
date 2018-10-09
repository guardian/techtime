
# encoding: UTF-8

# ------------------------------------------------------------
# utils

require 'sinatra'
# http://www.sinatrarb.com/intro.html

require 'securerandom'
# SecureRandom.hex    #=> "eb693ec8252cd630102fd0d0fb7c3485"
# SecureRandom.hex(2) #=> "eb69"
# SecureRandom.uuid   #=> "2d931510-d99f-494a-8c67-87feb05e1594"

require 'digest/sha1'
# Digest::SHA1.hexdigest 'foo'
# Digest::SHA1.file(myFile).hexdigest

require 'json'

require 'find'

require 'fileutils'
# FileUtils.mkpath '/a/b/c'
# FileUtils.cp(src, dst)
# FileUtils.mv 'oldname', 'newname'
# FileUtils.rm(path_to_image)
# FileUtils.rm_rf('dir/to/remove')

require 'time'

require 'digest/sha1'
# Digest::SHA1.hexdigest 'foo'
# Digest::SHA1.file(myFile).hexdigest

# --  --------------------------------------------------

require_relative "library/BombsUtils.rb"
require_relative "library/MapUtils.rb"
require_relative "library/Navigation.rb"
require_relative "library/ScoringUtils.rb"
require_relative "library/UserKeys.rb"
require_relative "library/UserFleet.rb"
require_relative "library/Throttling.rb"

# --  --------------------------------------------------

LUCILLE_INSTANCE = ENV["COMPUTERLUCILLENAME"]

if LUCILLE_INSTANCE.nil? then
    puts "Error: Environment variable 'COMPUTERLUCILLENAME' is not defined."
    exit
end

# -- --------------------------------------------------

SERVER_FOLDERPATH = File.dirname(__FILE__)

# -- --------------------------------------------------

require_relative "GameLibrary.rb"

# -- --------------------------------------------------

GAME_DATA_FOLDERPATH = "/Galaxy/DataBank/WeeklyCodingChallenges/20180920-Weekly/#{LUCILLE_INSTANCE}"
GAME_PARAMETERS_FILEPATH = File.dirname(__FILE__) + "/game-parameters.json"
$GAME_PARAMETERS = JSON.parse(IO.read(GAME_PARAMETERS_FILEPATH)) # This is the first load, the file is duplicated and (re)read when a new map is created

# -- --------------------------------------------------

$usersFleetsIOActionsMutex = Mutex.new
$mapInitMutex = Mutex.new

# -- --------------------------------------------------

File.open("#{GAME_DATA_FOLDERPATH}/server-last-restart-datetime.txt", "w"){|f| f.print(Time.new.utc.iso8601) }

# -- --------------------------------------------------
# Route

=begin

    HTTP error codes:
        401 Unauthorized
        403 Forbidden
        404 Not Found

=end

set :port, 14561
#set :public_folder, "path/to/www"

not_found do
  '404'
end

get '/' do
    content_type 'text/plain'
    [
        "Space Battle. Game Server. Running at #{LUCILLE_INSTANCE}",
        "See https://github.com/guardian/techtime/tree/master/Coding%20Challenges/20180920-Weekly for details."
    ].join("\n") + "\n"
end

# ------------------------------------------
# Some admin

get '/game/v1/get-userkey/:username' do
    content_type 'text/plain'
    username = params["username"]

    if username.include?(":") then
        return JSON.generate(GameLibrary::makeErrorAnswer(403, "b357013e", "Usernames cannot contain a colon (character ':')"))
    end

    userKeysData = UserKeys::getUserKeysData()
    if userKeysData.any?{|record| record[0]==username } then
        return JSON.generate(GameLibrary::makeErrorAnswer(403, "bb7822f2", "There has already been a userkey issued for this username. If you think this is a mistake or you have forgotten your userkey, please contact Pascal."))
    else
        userkey = SecureRandom.hex(8)
        UserKeys::commitUserKey(username, userkey)
        [
            "username: #{username}",
            "userkey : #{userkey}"
        ].join("\n") + "\n"
    end
end

# ------------------------------------------
# Map and Game Parameters

get '/game/v1/map' do
    content_type 'application/json'
    JSON.generate(MapUtils::getCurrentMap())
end

get '/game/v1/parameters' do
    content_type 'application/json'
    JSON.pretty_generate($GAME_PARAMETERS)
end

# ------------------------------------------
# User Fleet Actions 

get '/game/v1/:userkey/:mapid/capital-ship/init' do

    content_type 'application/json'

    userkey = params["userkey"]
    mapId = params["mapid"]

    currentHour = GameLibrary::hourCode()

    # ------------------------------------------------------
    # Throttling

    Throttling::throttle(userkey)

    # ------------------------------------------------------
    # User Credentials and Map Validity Checks

    username = UserKeys::getUsernameFromUserkeyOrNull(userkey)

    if username.nil? then
        return JSON.generate(GameLibrary::makeErrorAnswer(401, "c26b7c33", "Invalid userkey"))
    end

    if MapUtils::getCurrentMap()["mapId"] != mapId then
        return JSON.generate(GameLibrary::makeErrorAnswer(404, "6cd08e91", "Map not found (mapId is incorrect or outdated)"))
    end

    # ------------------------------------------------------
    # User Fleet validation

    if UserFleet::getUserFleetDataOrNull(currentHour, username) then
        return JSON.generate(GameLibrary::makeErrorAnswer(403, "3b6f4992", "You cannot init a Capital Ship, you already have one for this hour"))
    end

    # ------------------------------------------------------

    mapPoint = MapUtils::getCurrentMap()["points"].sample
    capitalShipInitialEnergy = $GAME_PARAMETERS["fleetCapitalShipInitialEnergyLevel"]
    topUpChallengeDifficulty = $GAME_PARAMETERS["fleetCapitalShipTopUpChallengeDifficulty"]
    userFleet = UserFleet::spawnUserFleet(username, mapPoint, capitalShipInitialEnergy, topUpChallengeDifficulty)

    UserFleet::commitFleetToDisk(currentHour, username, userFleet)

    JSON.generate(GameLibrary::make200Answer(nil, currentHour, username))
end

get '/game/v1/:userkey/:mapid/fleet' do

    content_type 'application/json'

    userkey = params["userkey"]
    mapId = params["mapid"]

    currentHour = GameLibrary::hourCode()

    # ------------------------------------------------------
    # Throttling

    Throttling::throttle(userkey)

    # ------------------------------------------------------
    # User Credentials and Map Validity Checks

    username = UserKeys::getUsernameFromUserkeyOrNull(userkey)

    if username.nil? then
        return JSON.generate(GameLibrary::makeErrorAnswer(401, "c26b7c33", "Invalid userkey"))
    end

    if MapUtils::getCurrentMap()["mapId"] != mapId then
        return JSON.generate(GameLibrary::makeErrorAnswer(404, "6cd08e91", "Map not found (mapId is incorrect or outdated)"))
    end

    # ------------------------------------------------------
    # User Fleet validation

    if UserFleet::getUserFleetDataOrNull(currentHour, username).nil? then
        return JSON.generate(GameLibrary::makeErrorAnswer(403, "95a0b4e5", "You do not yet have a fleet for this hour. (You should initiate one.)"))
    end

    JSON.generate(GameLibrary::make200Answer(nil, currentHour, username))
end

get '/game/v1/:userkey/:mapid/capital-ship/top-up/:code' do

    content_type 'application/json'

    userkey = params["userkey"]
    mapId = params["mapid"]
    code = params["code"]

    currentHour = GameLibrary::hourCode()

    # ------------------------------------------------------
    # Throttling

    Throttling::throttle(userkey)

    # ------------------------------------------------------
    # User Credentials and Map Validity Checks

    username = UserKeys::getUsernameFromUserkeyOrNull(userkey)

    if username.nil? then
        return JSON.generate(GameLibrary::makeErrorAnswer(401, "c26b7c33", "Invalid userkey"))
    end

    if MapUtils::getCurrentMap()["mapId"] != mapId then
        return JSON.generate(GameLibrary::makeErrorAnswer(404, "6cd08e91", "Map not found (mapId is incorrect or outdated)"))
    end

    # ------------------------------------------------------
    # User Fleet validation

    userFleet = UserFleet::getUserFleetDataOrNull(currentHour, username)

    if userFleet.nil? then
        return JSON.generate(GameLibrary::makeErrorAnswer(403, "95a0b4e5", "You do not yet have a fleet for this hour. (You should initiate one.)"))
    end

    if !userFleet["ships"][0]["alive"] then
        return JSON.generate(GameLibrary::makeErrorAnswer(403, "86877586", "Your capital ship for this hour is dead"))
    end

    # ------------------------------------------------------    

    if UserFleet::validateTopUpCode(currentHour, username, code) then
        if userFleet["ships"][0]["energyLevel"] + $GAME_PARAMETERS["fleetCapitalShipTopUpEnergyValue"] <= $GAME_PARAMETERS["fleetShipsMaxEnergy"]["capitalShip"] then
            topUpEnergyValue = $GAME_PARAMETERS["fleetCapitalShipTopUpEnergyValue"]
            difficulty = $GAME_PARAMETERS["fleetCapitalShipTopUpChallengeDifficulty"]
            UserFleet::topUpCapitalShipAndResetTopUpChallenge(currentHour, username, topUpEnergyValue, difficulty)
            JSON.generate(GameLibrary::make200Answer(nil, currentHour, username))
        else
            JSON.generate(GameLibrary::makeErrorAnswer(403, "d7713626", "Your code is correct, please keep it (!), but you cannot submit it at this time. Your ship has too much energy in reserve."))
        end
    else
        JSON.generate(GameLibrary::makeErrorAnswer(403, "d07feb9c", "Your code is not a solution to the challenge"))
    end
end

get '/game/v1/:userkey/:mapid/capital-ship/create-battle-cruiser' do

    content_type 'application/json'

    userkey = params["userkey"]
    mapId = params["mapid"]

    currentHour = GameLibrary::hourCode()

    # ------------------------------------------------------
    # Throttling

    Throttling::throttle(userkey)

    # ------------------------------------------------------
    # User Credentials and Map Validity Checks

    username = UserKeys::getUsernameFromUserkeyOrNull(userkey)

    if username.nil? then
        return JSON.generate(GameLibrary::makeErrorAnswer(401, "c26b7c33", "Invalid userkey"))
    end

    if MapUtils::getCurrentMap()["mapId"] != mapId then
        return JSON.generate(GameLibrary::makeErrorAnswer(404, "6cd08e91", "Map not found (mapId is incorrect or outdated)"))
    end

    # ------------------------------------------------------
    # User Fleet validation

    userFleet = UserFleet::getUserFleetDataOrNull(currentHour, username)

    if userFleet.nil? then
        return JSON.generate(GameLibrary::makeErrorAnswer(403, "95a0b4e5", "You do not yet have a fleet for this hour. (You should initiate one.)"))
    end

    if !userFleet["ships"][0]["alive"] then
        return JSON.generate(GameLibrary::makeErrorAnswer(403, "86877586", "Your capital ship for this hour is dead"))
    end

    # ------------------------------------------------------

    battleCruiserBuildEnergyCost = $GAME_PARAMETERS["fleetBattleCruiserBuildEnergyCost"]
    battleCruiserInitialEnergyLevel = $GAME_PARAMETERS["fleetBattleCruiserInitialEnergyLevel"]

    userFleet = UserFleet::getUserFleetDataOrNull(currentHour, username)
    capitalShipCanPerformBattleShipCreation = userFleet["ships"][0]["energyLevel"] >= ( battleCruiserBuildEnergyCost + battleCruiserInitialEnergyLevel )
    if capitalShipCanPerformBattleShipCreation then
        userFleet["ships"][0]["energyLevel"] = userFleet["ships"][0]["energyLevel"] - ( battleCruiserBuildEnergyCost + battleCruiserInitialEnergyLevel )
        mapPoint = MapUtils::getCurrentMap()["points"].sample
        battleCruiser = UserFleet::spawnBattleCruiser(mapPoint, battleCruiserInitialEnergyLevel)
        userFleet["ships"] << battleCruiser
        UserFleet::commitFleetToDisk(currentHour, username, userFleet)
        JSON.generate(GameLibrary::make200Answer(battleCruiser, currentHour, username))
    else
        return JSON.generate(GameLibrary::makeErrorAnswer(403, "36be6a8b", "Your capital ship doesn't have enough energy to complete the construction of a battle cruiser. You have #{userFleet["ships"][0]["energyLevel"]} but you need #{(battleCruiserBuildEnergyCost+battleCruiserInitialEnergyLevel)}"))
    end

end

get '/game/v1/:userkey/:mapid/capital-ship/create-energy-carrier/:energyamount' do

    content_type 'application/json'

    userkey = params["userkey"]
    mapId = params["mapid"]

    energyamount = params["energyamount"].to_f

    currentHour = GameLibrary::hourCode()

    # ------------------------------------------------------
    # Throttling

    Throttling::throttle(userkey)

    # ------------------------------------------------------
    # User Credentials and Map Validity Checks

    username = UserKeys::getUsernameFromUserkeyOrNull(userkey)

    if username.nil? then
        return JSON.generate(GameLibrary::makeErrorAnswer(401, "c26b7c33", "Invalid userkey"))
    end

    if MapUtils::getCurrentMap()["mapId"] != mapId then
        return JSON.generate(GameLibrary::makeErrorAnswer(404, "6cd08e91", "Map not found (mapId is incorrect or outdated)"))
    end

    # ------------------------------------------------------
    # User Fleet validation

    userFleet = UserFleet::getUserFleetDataOrNull(currentHour, username)

    if userFleet.nil? then
        return JSON.generate(GameLibrary::makeErrorAnswer(403, "95a0b4e5", "You do not yet have a fleet for this hour. (You should initiate one.)"))
    end

    if !userFleet["ships"][0]["alive"] then
        return JSON.generate(GameLibrary::makeErrorAnswer(403, "86877586", "Your capital ship for this hour is dead"))
    end

    if energyamount > $GAME_PARAMETERS["fleetShipsMaxEnergy"]["energyCarrier"] then
        return JSON.generate(GameLibrary::makeErrorAnswer(403, "b68c3046", "You are creating a carrier with too much energy. Upper limit is #{$GAME_PARAMETERS["fleetShipsMaxEnergy"]["energyCarrier"]} units of energy."))    
    end

    # ------------------------------------------------------ 

    carrierBuildEnergyCost = $GAME_PARAMETERS["fleetEnergyCarrierBuildEnergyCost"]
    carrierInitialEnergyLevel = energyamount 

    userFleet = UserFleet::getUserFleetDataOrNull(currentHour, username)
    capitalShipCanPerformCarrierCreation = userFleet["ships"][0]["energyLevel"] >= ( carrierBuildEnergyCost + carrierInitialEnergyLevel )
    if capitalShipCanPerformCarrierCreation then
        userFleet["ships"][0]["energyLevel"] = userFleet["ships"][0]["energyLevel"] - ( carrierBuildEnergyCost + carrierInitialEnergyLevel )
        mapPoint = MapUtils::getCurrentMap()["points"].sample
        energyCarrier = UserFleet::spawnEnergyCarrier(mapPoint, carrierInitialEnergyLevel)
        userFleet["ships"]<< energyCarrier
        UserFleet::commitFleetToDisk(currentHour, username, userFleet)
        JSON.generate(GameLibrary::make200Answer(energyCarrier, currentHour, username))
    else
        return JSON.generate(GameLibrary::makeErrorAnswer(403, "fc31efd0", "Your capital ship doesn't have enough energy to complete the construction of an energy carrier carrying #{carrierInitialEnergyLevel}. You have #{userFleet["ships"][0]["energyLevel"]} but you need #{(carrierBuildEnergyCost+carrierInitialEnergyLevel)}"))
    end
end

get '/game/v1/:userkey/:mapid/jump/:shipuuid/:targetpointlabel' do

    content_type 'application/json'

    userkey = params["userkey"]
    mapId = params["mapid"]

    shipuuid = params["shipuuid"]
    targetPointLabel = params["targetpointlabel"]

    currentHour = GameLibrary::hourCode()

    # ------------------------------------------------------
    # Throttling

    Throttling::throttle(userkey)

    # ------------------------------------------------------
    # User Credentials and Map Validity Checks

    username = UserKeys::getUsernameFromUserkeyOrNull(userkey)

    if username.nil? then
        return JSON.generate(GameLibrary::makeErrorAnswer(401, "c26b7c33", "Invalid userkey"))
    end

    if MapUtils::getCurrentMap()["mapId"] != mapId then
        return JSON.generate(GameLibrary::makeErrorAnswer(404, "6cd08e91", "Map not found (mapId is incorrect or outdated)"))
    end

    # ------------------------------------------------------
    # Map Validation

    map = MapUtils::getCurrentMap()    

    targetMapPoint = MapUtils::getPointForlabelAtMapOrNull(targetPointLabel, map)
    if targetMapPoint.nil? then
        return JSON.generate(GameLibrary::makeErrorAnswer(404, "34d25d8a", "The specified point doesn't exist"))
    end

    # ------------------------------------------------------
    # User Fleet validation

    userFleet = UserFleet::getUserFleetDataOrNull(currentHour, username)

    if userFleet.nil? then
        return JSON.generate(GameLibrary::makeErrorAnswer(403, "95a0b4e5", "You do not yet have a fleet for this hour. (You should initiate one.)"))
    end

    # Need to check whether we own a ship of with that uuid, and retrieve it.
    ship = UserFleet::getShipPerUUIDOrNull(currentHour, username, shipuuid)
    if ship.nil? then
        return JSON.generate(GameLibrary::makeErrorAnswer(404, "acfec803", "Your fleet has no ship with this uuid"))
    end

    # Need to check whether the ship is alive ot not
    if !ship["alive"] then
        return JSON.generate(GameLibrary::makeErrorAnswer(403, "f7a8dee2", "The ship is dead"))
    end    

    # In the current version of the game energy carriers need the capital ship to be alive 
    # in order to be controlled. Therfore we record whether or not the capital is alive.

    if ship["nomenclature"] == "energyCarrier" then
        if !userFleet["ships"][0]["alive"] then
            return JSON.generate(GameLibrary::makeErrorAnswer(403, "03717296", "Your capital ship is dead. You cannot jump energy carriers in that case."))
        end 
    end

    sourceMapPoint = ship["location"]

    jec = Navigation::jumpEnergyCost(sourceMapPoint, targetMapPoint, ship["nomenclature"])

    # Need to check whether the ship has enough energy left to jump
    if ship["energyLevel"] < jec then
        return JSON.generate(GameLibrary::makeErrorAnswer(403, "c36b1859", "The ship doesn't have enough energy for this jump. Available: #{ship["energyLevel"]}. Required: #{jec}"))
    end    

    # ------------------------------------------------------
    
    # Now performing the jump
    ship["location"] = targetMapPoint
    ship["energyLevel"] = ship["energyLevel"] - jec

    userFleet = UserFleet::insertOrUpdateShipAtFleet(userFleet, ship)
    UserFleet::commitFleetToDisk(currentHour, username, userFleet)

    JSON.generate(GameLibrary::make200Answer(nil, currentHour, username))

end

get '/game/v1/:userkey/:mapid/energy-transfer/:ship1uuid/:ship2uuid/:amount' do

    content_type 'application/json'

    userkey = params["userkey"]
    mapId = params["mapid"]

    ship1uuid = params["ship1uuid"]
    ship2uuid = params["ship2uuid"]
    amountToTransfer = params["amount"].to_f

    currentHour = GameLibrary::hourCode()

    # ------------------------------------------------------
    # Throttling

    Throttling::throttle(userkey)

    # ------------------------------------------------------
    # User Credentials and Map Validity Checks

    username = UserKeys::getUsernameFromUserkeyOrNull(userkey)

    if username.nil? then
        return JSON.generate(GameLibrary::makeErrorAnswer(401, "c26b7c33", "Invalid userkey"))
    end

    if MapUtils::getCurrentMap()["mapId"] != mapId then
        return JSON.generate(GameLibrary::makeErrorAnswer(404, "6cd08e91", "Map not found (mapId is incorrect or outdated)"))
    end

    # ------------------------------------------------------
    # User Fleet validation

    if ship1uuid==ship2uuid then
        return JSON.generate(GameLibrary::makeErrorAnswer(403, "66474ae3", "You are transferring energy from a ship to itself."))
    end

    userFleet = UserFleet::getUserFleetDataOrNull(currentHour, username)

    if userFleet.nil? then
        return JSON.generate(GameLibrary::makeErrorAnswer(403, "95a0b4e5", "You do not yet have a fleet for this hour. (You should initiate one.)"))
    end

    ship1 = UserFleet::getShipPerUUIDOrNull(currentHour, username, ship1uuid)
    ship2 = UserFleet::getShipPerUUIDOrNull(currentHour, username, ship2uuid)

    if ship1.nil? then
        return JSON.generate(GameLibrary::makeErrorAnswer(404, "7b680a12", "Your fleet has no ship with uuid #{ship1uuid}"))
    end

    if ship2.nil? then
        return JSON.generate(GameLibrary::makeErrorAnswer(404, "1c5436b9", "Your fleet has no ship with uuid #{ship2uuid}"))
    end

    if !ship1["alive"] then
        return JSON.generate(GameLibrary::makeErrorAnswer(403, "391388ae", "The source ship, #{ship1uuid}, is dead"))
    end

    if !ship2["alive"] then
        return JSON.generate(GameLibrary::makeErrorAnswer(403, "a9e028ed", "The target ship, #{ship2uuid}, is dead"))
    end

    if ship2["location"]["label"] != ship1["location"]["label"] then
        return JSON.generate(GameLibrary::makeErrorAnswer(403, "a9971906", "You cannot transfer energy between the two ships, they are not at the same map location"))
    end

    if ship1["energyLevel"] == 0 then
        return JSON.generate(GameLibrary::makeErrorAnswer(403, "cf1e71c1", "The source ship has no energy to transfer"))
    end    

    amountToTransfer = [ amountToTransfer, $GAME_PARAMETERS["fleetShipsMaxEnergy"][ship2["nomenclature"]] - ship2["energyLevel"] ].min

    # ------------------------------------------------------

    ship2["energyLevel"] = ship2["energyLevel"] + amountToTransfer
    ship1["energyLevel"] = ship1["energyLevel"] - amountToTransfer

    userFleet = UserFleet::insertOrUpdateShipAtFleet(userFleet, ship1)
    userFleet = UserFleet::insertOrUpdateShipAtFleet(userFleet, ship2)
    UserFleet::commitFleetToDisk(currentHour, username, userFleet)

    JSON.generate(GameLibrary::make200Answer([ ship1, ship2 ], currentHour, username))

end

get '/game/v1/:userkey/:mapid/bomb/:battlecruisershipuuid/:targetpointlabel' do

    content_type 'application/json'

    userkey = params["userkey"]
    mapId = params["mapid"]

    attackerBattleCruiserShipUUID = params["battlecruisershipuuid"]
    targetpointlabel = params["targetpointlabel"]

    currentHour = GameLibrary::hourCode()

    # ------------------------------------------------------
    # Throttling

    Throttling::throttle(userkey)

    # ------------------------------------------------------
    # User Credentials and Map Validity Checks

    attackerUsername = UserKeys::getUsernameFromUserkeyOrNull(userkey)

    if attackerUsername.nil? then
        return JSON.generate(GameLibrary::makeErrorAnswer(401, "c26b7c33", "Invalid userkey"))
    end

    if MapUtils::getCurrentMap()["mapId"] != mapId then
        return JSON.generate(GameLibrary::makeErrorAnswer(404, "6cd08e91", "Map not found (mapId is incorrect or outdated)"))
    end

    # ------------------------------------------------------
    # Map Validation

    map = MapUtils::getCurrentMap()

    targetMapPoint = MapUtils::getPointForlabelAtMapOrNull(targetpointlabel, map)
    if targetMapPoint.nil? then
        return JSON.generate(GameLibrary::makeErrorAnswer(404, "88bb18fd", "The specified point doesn't exist"))
    end

    # ------------------------------------------------------
    # User Fleet validation

    attackerUserFleet = UserFleet::getUserFleetDataOrNull(currentHour, attackerUsername)

    if attackerUserFleet.nil? then
        return JSON.generate(GameLibrary::makeErrorAnswer(403, "95a0b4e5", "You do not yet have a fleet for this hour. (You should initiate one.)"))
    end

    attackerBattleCruiser = UserFleet::getShipPerUUIDOrNull(currentHour, attackerUsername, attackerBattleCruiserShipUUID)

    if attackerBattleCruiser.nil? then
        return JSON.generate(GameLibrary::makeErrorAnswer(404, "1a0ddb98", "Your fleet has no ship with uuid #{attackerBattleCruiserShipUUID}"))
    end

    if !attackerBattleCruiser["alive"] then
        return JSON.generate(GameLibrary::makeErrorAnswer(403, "bc0bb00f", "Your attacking battle cruiser is dead"))
    end

    # ------------------------------------------------------
    # At this point we can attempt shooting

    if attackerBattleCruiser["energyLevel"] < ( $GAME_PARAMETERS["fleetBattleCruiserBombBuildingCost"] + $GAME_PARAMETERS["fleetBattleCruiserBombNominalEnergy"] ) then
        return JSON.generate(GameLibrary::makeErrorAnswer(403, "943802d8", "Your attacking battle cruiser doesn't have enough energy to complete the construction of a bomb"))   
    end

    attackerBattleCruiser["energyLevel"] = attackerBattleCruiser["energyLevel"] - ( $GAME_PARAMETERS["fleetBattleCruiserBombBuildingCost"] + $GAME_PARAMETERS["fleetBattleCruiserBombNominalEnergy"] )
    attackerUserFleet = UserFleet::insertOrUpdateShipAtFleet(attackerUserFleet, attackerBattleCruiser)
    UserFleet::commitFleetToDisk(currentHour, attackerUsername, attackerUserFleet)

    # Shooting happened from the point of view of the attacker (attacker user fleet is not yet to disk)

    # ------------------------------------------------------
    # Ok, now time to do damage

    distanceToTargetPoint = MapUtils::distanceBetweenTwoMapPoints(attackerBattleCruiser["location"], targetMapPoint)
    bombEffectiveEnergy = BombsUtils::bombEffectiveEnergy($GAME_PARAMETERS["fleetBattleCruiserBombNominalEnergy"], distanceToTargetPoint)

    attackerAllShipsDamageReport = []

    GameLibrary::userFleetsForHour(currentHour)
        .each{|targetPlayerXUserFleet|
            UserFleet::userShipsWithinDisk(currentHour, targetPlayerXUserFleet["username"], attackerBattleCruiser["location"], 0)
                .each{|targetShipX|
                    targetPlayerXUserFleet, targetShipX, damageCausedOnTargetShipXForAttackerPlayerReport = UserFleet::registerShipTakingBombImpact(targetPlayerXUserFleet, attackerBattleCruiser["location"], attackerUsername, targetShipX, bombEffectiveEnergy)
                    attackerAllShipsDamageReport << damageCausedOnTargetShipXForAttackerPlayerReport
                    targetPlayerXUserFleet = UserFleet::insertOrUpdateShipAtFleet(targetPlayerXUserFleet, targetShipX)
                }
            UserFleet::commitFleetToDisk(currentHour, targetPlayerXUserFleet["username"], targetPlayerXUserFleet)
            # Target players fleet have been updated. One clean call to UserFleet::commitFleetToDisk after having updated every ship at the bombing location   
        }


    # ------------------------------------------------------
    # Now we only need to compute any point increase for the attacker and commit the attacker fleet to disk

    # attackerAllShipsDamageReport contains either nil or this
    #{
    #    "username"     => userFleet["username"],
    #    "nomenclature" => targetShip["nomenclature"],
    #    "alive"        => targetShip["alive"]
    #}    

    attackerAllShipsDamageReport = attackerAllShipsDamageReport.compact # better    
    attackerAllShipsDamageReport
    .select{|item|
        !item["alive"]
    }
    .each{|item|
        GameLibrary::doUserFleetPointIncreaseForShipDestroyed(currentHour, attackerUsername, item["nomenclature"])
    }
    
    JSON.generate(GameLibrary::make200Answer(attackerAllShipsDamageReport, currentHour, attackerUsername))
end

get '/game/v1/:userkey/:mapid/space-probe/:battlecruisershipuuid' do

    content_type 'application/json'

    userkey = params["userkey"]
    mapId = params["mapid"]

    battleCruiserShipUUID = params["battlecruisershipuuid"]

    currentHour = GameLibrary::hourCode()

    # ------------------------------------------------------
    # Throttling

    Throttling::throttle(userkey)

    # ------------------------------------------------------
    # User Credentials and Map Validity Checks

    username = UserKeys::getUsernameFromUserkeyOrNull(userkey)

    if username.nil? then
        return JSON.generate(GameLibrary::makeErrorAnswer(401, "c26b7c33", "Invalid userkey"))
    end

    if MapUtils::getCurrentMap()["mapId"] != mapId then
        return JSON.generate(GameLibrary::makeErrorAnswer(404, "6cd08e91", "Map not found (mapId is incorrect or outdated)"))
    end

    # ------------------------------------------------------
    # User Fleet validation

    userFleet = UserFleet::getUserFleetDataOrNull(currentHour, username)

    if userFleet.nil? then
        return JSON.generate(GameLibrary::makeErrorAnswer(403, "95a0b4e5", "You do not yet have a fleet for this hour. (You should initiate one.)"))
    end

    battleCruiser = UserFleet::getShipPerUUIDOrNull(currentHour, username, battleCruiserShipUUID)

    if battleCruiser.nil? then
        return JSON.generate(GameLibrary::makeErrorAnswer(404, "a0ce7e39", "Your fleet has no ship with uuid #{battleCruiserShipUUID}"))
    end

    if !battleCruiser["alive"] then
        return JSON.generate(GameLibrary::makeErrorAnswer(403, "051366e2", "The probing battle cruiser is dead"))
    end

    # ------------------------------------------------------
    # At this point we can attempt shooting

    spaceProbeResults = {
        "unixtime" => Time.new.to_f,
        "datetime" => Time.now.utc.iso8601,
        "results"  => []
    }

    GameLibrary::userFleetsForHour(currentHour)
        .each{|otherPlayerUserFleet|
            next if otherPlayerUserFleet["username"] == username
            UserFleet::userShipsWithinDisk(currentHour, otherPlayerUserFleet["username"], battleCruiser["location"], 300)
                .each{|ship|
                    spaceProbeResultItem = {
                        "location" => ship["location"],
                        "nomenclature" => ship["nomenclature"],
                        "username" => otherPlayerUserFleet["username"]
                    }
                    spaceProbeResults["results"] << spaceProbeResultItem
                }
        }

    userFleet["spaceProbeResults"][battleCruiser["uuid"]] = spaceProbeResults

    UserFleet::commitFleetToDisk(currentHour, username, userFleet)

    JSON.generate(GameLibrary::make200Answer(spaceProbeResults, currentHour, username))
end

get '/game/v1/scores' do

    content_type 'text/plain'

    users = {}

    addScoreToUserLambda = lambda {|users, user, score|
        if users[user].nil? then
            users[user] = 0
        end
        users[user] = (users[user] + score).round(3)
        users
    }

    [
        GameLibrary::getGameAtHoursDataFolderPaths()
            .sort
            .map{|hoursFolderpath|
                currentHour = File.basename(hoursFolderpath)
                userFleetsOrdered = GameLibrary::userFleetsForHour(currentHour)
                    .sort{|f1, f2| f1["gameScore"] <=> f2["gameScore"] }
                    .reverse
                score = 0.1/0.7
                lastValue = nil
                [
                    "",
                    File.basename(hoursFolderpath),
                    userFleetsOrdered.map{|userFleet|
                        currentUserValue = userFleet["gameScore"]
                        if currentUserValue != lastValue then
                            score = score*0.7 
                        end
                        lastValue = currentUserValue
                        users = addScoreToUserLambda.call(users, userFleet["username"], score)
                        "#{userFleet["username"].ljust(20)} , game score: #{"%10.3f" % currentUserValue} , leaderboard score increment: #{score.round(3)}"
                    }.join("\n")
                ].join("\n")
            }.join("\n") + "\n",
        "Summary: ",    
        users
            .keys
            .map{|username| [username, users[username]] }
            .sort{|p1,p2| p1[1] <=> p2[1] }
            .reverse
            .map{|p|
                username, score = p
                "   - #{username.ljust(20)} : #{score}"
            }.join("\n")
    ].join("\n") + "\n"

end

# --------------------------------------------------------------
# Challenge 20180927

CHALLENGE_DATA_FOLDERPATH = "/Galaxy/DataBank/WeeklyCodingChallenges/20180927-Weekly/#{LUCILLE_INSTANCE}"
if !File.exists?(CHALLENGE_DATA_FOLDERPATH) then
    FileUtils.mkpath(CHALLENGE_DATA_FOLDERPATH)
end

$STRUCTURE = nil

=begin

Structure: Map[HourCode, StructureHour]

StructureHour: {
    "map": Map
    "userSubmissions": Map[Username, UserSubmissionsItem]
}

UserSubmissionsItem: {
    "username"   : String
    "submission" : String
    "value"      : Float
}

Disk: {
    "point"  : MapPoint
    "radius" : Float
}

=end

class Challenge20180927

    # Challenge20180927::challengeStructureFilepathForHourCode(hourCode)
    def self.challengeStructureFilepathForHourCode(hourCode)
        "#{CHALLENGE_DATA_FOLDERPATH}/structure-#{hourCode}.json"
    end

    # Challenge20180927::issueNewStructure()
    def self.issueNewStructure()
        $STRUCTURE = {}
        map = MapUtils::getCurrentMap()
        map["points"] = map["points"].first(100)
        $STRUCTURE["map"] = map
        $STRUCTURE["userSubmissions"] = {}
        Challenge20180927::commitStructureToDisk() 
    end

    # Challenge20180927::ensureStructure()
    def self.ensureStructure()
        if $STRUCTURE.nil? then
            currentHour = GameLibrary::hourCode()
            filepath = Challenge20180927::challengeStructureFilepathForHourCode(currentHour)
            if File.exists?(filepath) then
                structure = JSON.parse(IO.read(filepath))
                if structure["map"]["timestamp"] == currentHour then
                    $STRUCTURE = structure
                    return
                end
            else
                Challenge20180927::issueNewStructure()
                return                
            end
        end
        if $STRUCTURE and ( $STRUCTURE["map"]["timestamp"] == GameLibrary::hourCode() ) then
            return
        end
        Challenge20180927::issueNewStructure()
    end

    # Challenge20180927::commitStructureToDisk()
    def self.commitStructureToDisk()
        hourCode = GameLibrary::hourCode()
        return if $STRUCTURE.nil?
        filepath = Challenge20180927::challengeStructureFilepathForHourCode(hourCode)
        File.open(filepath, "w"){ |f| f.puts(JSON.pretty_generate($STRUCTURE)) }
    end

    # Challenge20180927::hourCodesFromTimeline()
    def self.hourCodesFromTimeline()
        Dir.entries(CHALLENGE_DATA_FOLDERPATH)
            .select{|filename| filename[-5, 5]==".json" }
            .map{|filename| filename[10, 13]}
    end

    # Challenge20180927::getStructureForGivenHourCodeOrNull(hourCode)
    def self.getStructureForGivenHourCodeOrNull(hourCode)
        if hourCode == $STRUCTURE["map"]["timestamp"] then
            $STRUCTURE
        else
            filepath = Challenge20180927::challengeStructureFilepathForHourCode(hourCode)
            return nil if !File.exists?(filepath)
            JSON.parse(IO.read(filepath))
        end

    end

    # Challenge20180927::adjustRadiusDown(radius)
    def self.adjustRadiusDown(radius)
        (radius*(10**12)).to_i.to_f/(10**12)
    end

    # Challenge20180927::makeDisksOrNull(map, submission)
    def self.makeDisksOrNull(map, submission)
        disks  = []
        labelsAndRadii = []
        tokens = submission.split(",").map{|t| t.strip }
        while tokens.size >= 2 do
            label  = tokens.shift
            radius = tokens.shift
            radius = radius.to_f
            mapPoint = MapUtils::getPointForlabelAtMapOrNull(label, map)
            return nil if mapPoint.nil?
            disk = {
                "point"  => mapPoint,
                "radius" => Challenge20180927::adjustRadiusDown(radius)
            }
            disks << disk
        end
        disks
    end

    # Challenge20180927::hasLessThan50Disks(disks)
    def self.hasLessThan50Disks(disks)
        disks.size <= 50 
    end

    # Challenge20180927::diskDoesNotIntersectMapNonTrivially(map, disk)
    def self.diskDoesNotIntersectMapNonTrivially(map, disk)
        points = map["points"].select{|point| point["label"] != disk["point"]["label"] }
        points.all?{|point| MapUtils::distanceBetweenTwoMapPoints(point, disk["point"]) >= disk["radius"] }
    end

    # Challenge20180927::individualDisksAreValid(map, disks)
    def self.individualDisksAreValid(map, disks)
        disks.all?{|disk| Challenge20180927::diskDoesNotIntersectMapNonTrivially(map, disk) }
    end

    # Challenge20180927::collectionIsValid(disks)
    def self.collectionIsValid(disks)
        disks.combination(2).to_a.all?{|pair|
            disk1 = pair[0]
            disk2 = pair[1]
            MapUtils::distanceBetweenTwoMapPoints(disk1["point"], disk2["point"]) >= (disk1["radius"]+disk2["radius"])
        }
    end

    # Challenge20180927::collectionValue(disks)
    def self.collectionValue(disks)
        disks.map{|disk| disk["radius"]**2 }.inject(0, :+)
    end

end

Thread.new {
    loop {
        sleep 60
        Challenge20180927::commitStructureToDisk()
    }
}

get '/challenge-20180927/map' do
    content_type 'application/json'
    Challenge20180927::ensureStructure()
    JSON.generate($STRUCTURE["map"])  
end

get '/challenge-20180927/submit/:mapid/:username/:submission' do
    content_type 'application/json'
    mapid = params["mapid"]
    username = params["username"]
    submission = params["submission"]
    Challenge20180927::ensureStructure()
    currentHour = GameLibrary::hourCode()
    map = $STRUCTURE["map"]
    if mapid != map["mapId"] then
        status 404
        return "Incorrect map identifier \n"
    end
    disks = Challenge20180927::makeDisksOrNull(map, submission)
    if disks.nil? then
        status 403
        return "403: Your submission does not resolve for for this map \n"
    end
    if !Challenge20180927::hasLessThan50Disks(disks) then
        status 403
        return "403: You are submitting too many disks \n"
    end
    if !Challenge20180927::individualDisksAreValid(map, disks) then
        status 403
        return "403: At least one of your disks is not valid (intersects the map in non trivial ways) \n"
    end
    if !Challenge20180927::collectionIsValid(disks) then
        status 403
        return "403: Your collection is not valid (intersecting disks) \n"
    end
    puts JSON.generate(disks)
    collectionValue = Challenge20180927::collectionValue(disks)
    if $STRUCTURE["userSubmissions"][username].nil? then
        $STRUCTURE["userSubmissions"][username] = {
            "username"   => username,
            "submission" => submission,
            "value"      => collectionValue
        }
        JSON.generate(["Well done!"])
    else
        onRecordCollectionValue = $STRUCTURE["userSubmissions"][username]["value"]
        if collectionValue > onRecordCollectionValue then
            $STRUCTURE["userSubmissions"][username] = {
                "username"   => username,
                "submission" => submission,
                "value"      => collectionValue
            }
            JSON.generate(["Well done!"])
        else
            JSON.generate(["Ok"])
        end
    end

end

get '/challenge-20180927/scores' do
    content_type 'text/plain'
    Challenge20180927::ensureStructure()
    users = {}
    addScoreToUserLambda = lambda {|users, user, score|
        if users[user].nil? then
            users[user] = 0
        end
        users[user] = (users[user] + score).round(3)
        users
    }

    [
        Challenge20180927::hourCodesFromTimeline()
            .sort
            .map{|hourCode|
                puts hourCode
                structure = Challenge20180927::getStructureForGivenHourCodeOrNull(hourCode)
                usersubmissionitemsordered = structure["userSubmissions"].values
                    .sort{|us1, us2| us1["value"] <=> us2["value"] }
                    .reverse
                score = 0.1/0.7
                lastValue = nil
                [
                    "",
                    hourCode,
                    usersubmissionitemsordered.map{|item|
                        currentUserValue = item["value"]
                        if currentUserValue != lastValue then
                            score = score*0.7 
                        end
                        lastValue = currentUserValue
                        users = addScoreToUserLambda.call(users, item["username"], score)
                        "#{item["username"].ljust(20)} , game score: #{"%10.3f" % currentUserValue} , leaderboard score increment: #{score.round(3)}"
                    }.join("\n")
                ].join("\n")
            }.join("\n") + "\n",
        "Summary: ",    
        users
            .keys
            .map{|username| [username, users[username]] }
            .sort{|p1,p2| p1[1] <=> p2[1] }
            .reverse
            .map{|p|
                username, score = p
                "   - #{username.ljust(20)} : #{score}"
            }.join("\n")
    ].join("\n") + "\n"

end

# --------------------------------------------------------------

