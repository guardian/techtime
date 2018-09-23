
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
require_relative "library/UserKeys.rb"
require_relative "library/UserFleet.rb"

# --  --------------------------------------------------

LUCILLE_INSTANCE = ENV["COMPUTERLUCILLENAME"]

if LUCILLE_INSTANCE.nil? then
    puts "Error: Environment variable 'COMPUTERLUCILLENAME' is not defined."
    exit
end

# -- --------------------------------------------------

SERVER_FOLDERPATH = File.dirname(__FILE__)

class DeploymentOperator
    # DeploymentOperator::folderHash(folderpath)
    def self.folderHash(folderpath)
        Dir.entries(folderpath)
            .select{|filename| filename[0,1] != "." }
            .map{|filename| "#{folderpath}/#{filename}" }
            .select{|filepath| File.file?(filepath) }
            .map{|filepath| "#{filepath}:#{Digest::SHA1.file(filepath).hexdigest}" }
            .join("::")
    end
    # DeploymentOperator::codeHash()
    def self.codeHash()
        hash1 = DeploymentOperator::folderHash(SERVER_FOLDERPATH)
        hash2 = DeploymentOperator::folderHash("#{SERVER_FOLDERPATH}/library")
        Digest::SHA1.hexdigest([hash1, hash2].join())
    end
end

$INITIAL_CODE_HASH = DeploymentOperator::codeHash()

=begin
    The logic here is that the server is keep alive by MacOSX's Launchd, so it is safe to 
    have it kill itself when the code is updated. Knowing it will (almost instantly) come back to life
    on the updated code. Hence "deployment".
=end

Thread.new {
    loop {
        sleep 120
        if $INITIAL_CODE_HASH != DeploymentOperator::codeHash() then
            exit
        end 
    }
}

# -- --------------------------------------------------

class GameLibrary

    # GameLibrary::hourCode()
    def self.hourCode()
        Time.new.strftime("%Y-%m-%d-%H")
    end

    # GameLibrary::getHoursFolderPaths()
    def self.getHoursFolderPaths()
        Dir.entries("#{GAME_DATA_FOLDERPATH}/Timeline")
            .select{|filename| filename[0,1]!="." }
            .map{|filename| "#{GAME_DATA_FOLDERPATH}/Timeline/#{filename}" }
    end

    # GameLibrary::getMapAtHourFolderpath(folderpath)
    def self.getMapAtHourFolderpath(folderpath)
        mapfilepath = "#{folderpath}/map.json"
        JSON.parse(IO.read(mapfilepath))
    end

    # GameLibrary::ensureGameFolderSetUpForThisHour()
    def self.ensureGameFolderSetUpForThisHour()

        currentHour = GameLibrary::hourCode()

        folderpath = "#{GAME_DATA_FOLDERPATH}/Timeline/#{currentHour}"
        if !File.exists?(folderpath) then
            FileUtils.mkpath folderpath
        end

        mapfilepath = "#{folderpath}/map.json"
        return folderpath if File.exists?(mapfilepath)

        # ---------------------------------------
        # The Map
        map = {}
        map["mapId"] = SecureRandom.uuid
        map["timestamp"] = currentHour
        map["points"] = (1..$GAME_PARAMETERS["mapJumpPointsCardinality"]).map{|indx|
            {
                "label" => SecureRandom.hex(4),
                "coordinates" => [ rand * $GAME_PARAMETERS["mapSize"], rand * $GAME_PARAMETERS["mapSize"] ].map{|c| c.round(2) }
            }
        }
        File.open(mapfilepath, "w"){ |f| f.puts(JSON.pretty_generate(map)) }

        # ---------------------------------------
        # Game Parameters
        FileUtils.cp(GAME_PARAMETERS_FILEPATH, "#{folderpath}/game-parameters.json")

        # ---------------------------------------
        # The BBC Fleet
        # Here we set up the BBC fleet

        username = "The BBC"
        mapPoint = map["points"].sample
        capitalShipInitialEnergy = $GAME_PARAMETERS["fleetCapitalShipInitialEnergyLevel"]
        topUpChallengeDifficulty = $GAME_PARAMETERS["fleetCapitalShipTopUpChallengeDifficulty"]
        userFleet = UserFleet::spawnUserFleet(username, mapPoint, capitalShipInitialEnergy, topUpChallengeDifficulty)
        (1..20).each{|index|
            mapPointX = map["points"].sample
            initialEnergyLevelX = 100 + rand*100
            cruiser = UserFleet::spawnBattleCruiser(mapPointX, initialEnergyLevelX)
            userFleet = UserFleet::insertOrUpdateShipAtFleet(userFleet, cruiser)
        }
        UserFleet::commitFleetToDisk(currentHour, username, userFleet)

        # ---------------------------------------

        folderpath

    end

    # GameLibrary::doUserFleetPointIncreaseForShipDestroyed(currentHour, username, nomenclature)
    def self.doUserFleetPointIncreaseForShipDestroyed(currentHour, username, nomenclature)
        userFleet = UserFleet::getUserFleetDataOrNull(currentHour, username)
        userFleet = ScoringUtils::userFleetPointIncreaseForShipDestroyed(userFleet, nomenclature)
        UserFleet::commitFleetToDisk(currentHour, username, fleet)
    end

    # GameLibrary::userFleetsForHour(currentHour)
    def self.userFleetsForHour(currentHour)
        Dir.entries("#{GAME_DATA_FOLDERPATH}/Timeline/#{currentHour}/fleets")
            .select{|filename| filename[-5,5]==".json" }
            .map{|filename| "#{GAME_DATA_FOLDERPATH}/Timeline/#{currentHour}/fleets/#{filename}" }
            .map{|filepath| JSON.parse(IO.read(filepath)) }
    end

    # GameLibrary::make200Answer(answer, currentHour, username)
    def self.make200Answer(answer, currentHour, username)
        {
            "status" => 200,
            "answer" => answer,
            "userFleet" => UserFleet::getUserFleetDataOrNull(currentHour, username)
        }
    end

    # GameLibrary::makeErrorAnswer(errorcode, errormessage)
    def self.makeErrorAnswer(errorcode, errormessage)
        {
            "status" => errorcode,
            "message" => errormessage
        }
    end

end

class Throttling

    # Throttling::userRequestCanProceed(username)
    def self.userRequestCanProceed(username)
        if $LastUserRequestsTimesForThrottling[username].nil? then
            true
        else
            (Time.new.to_f-$LastUserRequestsTimesForThrottling[username]) >= $GAME_PARAMETERS["serverThrottlingWaitingPeriodInSeconds"]
        end
    end

    # Throttling::updateUserActionTime(username)
    def self.updateUserActionTime(username)
        $LastUserRequestsTimesForThrottling[username] = Time.new.to_f
    end

end

# -- --------------------------------------------------

GAME_DATA_FOLDERPATH = "/Galaxy/DataBank/WeeklyCodingChallenges/20180920-Weekly/#{LUCILLE_INSTANCE}"
GAME_PARAMETERS_FILEPATH = File.dirname(__FILE__) + "/game-parameters.json"
$GAME_PARAMETERS = JSON.parse(IO.read(GAME_PARAMETERS_FILEPATH)) # This is the first load, the file is duplicated and (re)read when a new map is created
$LastUserRequestsTimesForThrottling = {}

# -- --------------------------------------------------



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
        return JSON.generate(GameLibrary::makeErrorAnswer(403, "Usernames cannot contain a colon (character ':')"))
    end

    userKeysData = UserKeys::getUserKeysData()
    if userKeysData.any?{|record| record[0]==username } then
        return JSON.generate(GameLibrary::makeErrorAnswer(403, "There has already been a userkey issued for this username. If you think this is a mistake or you have forgotten your userkey, please contact Pascal."))
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
    # User Credentials and Map Validity Checks

    username = UserKeys::getUsernameFromUserkeyOrNull(userkey)

    if username.nil? then
        return JSON.generate(GameLibrary::makeErrorAnswer(401, "Invalid userkey"))
    end

    if MapUtils::getCurrentMap()["mapId"] != mapId then
        return JSON.generate(GameLibrary::makeErrorAnswer(404, "Map not found (mapId is incorrect or outdated)"))
    end

    # ------------------------------------------------------
    # User Fleet validation

    if UserFleet::getUserFleetDataOrNull(currentHour, username) then
        return JSON.generate(GameLibrary::makeErrorAnswer(403, "You cannot init a Capital Ship, you already have one for this hour"))
    end

    # ------------------------------------------------------

    mapPoint = MapUtils::getCurrentMap()["points"].sample
    capitalShipInitialEnergy = $GAME_PARAMETERS["fleetCapitalShipInitialEnergyLevel"]
    topUpChallengeDifficulty = $GAME_PARAMETERS["fleetCapitalShipTopUpChallengeDifficulty"]
    userFleet = UserFleet::spawnUserFleet(username, mapPoint, capitalShipInitialEnergy, topUpChallengeDifficulty)

    UserFleet::commitFleetToDisk(currentHour, username, userFleet)

    JSON.generate(GameLibrary::make200Answer(nil, currentHour, username))
end

get '/game/v1/:userkey/:mapid/capital-ship/top-up/:code' do

    content_type 'application/json'

    userkey = params["userkey"]
    mapId = params["mapid"]
    code = params["code"]

    currentHour = GameLibrary::hourCode()

    # ------------------------------------------------------
    # User Credentials and Map Validity Checks

    username = UserKeys::getUsernameFromUserkeyOrNull(userkey)

    if username.nil? then
        return JSON.generate(GameLibrary::makeErrorAnswer(401, "Invalid userkey"))
    end

    if MapUtils::getCurrentMap()["mapId"] != mapId then
        return JSON.generate(GameLibrary::makeErrorAnswer(404, "Map not found (mapId is incorrect or outdated)"))
    end

    # ------------------------------------------------------
    # User Fleet validation

    userFleet = UserFleet::getUserFleetDataOrNull(currentHour, username)

    if userFleet.nil? then
        return JSON.generate(GameLibrary::makeErrorAnswer(404, "You do not yet have a fleet for this hour. (You should initiate one.)"))
    end

    if !userFleet["ships"][0]["alive"] then
        return JSON.generate(GameLibrary::makeErrorAnswer(403, "Your capital ship for this hour is dead"))
    end

    # ------------------------------------------------------    

    if UserFleet::validateTopUpCode(currentHour, username, code) then
        if userFleet["ships"][0]["energyLevel"] + $GAME_PARAMETERS["fleetCapitalShipTopUpEnergyValue"] <= $GAME_PARAMETERS["fleetShipsMaxEnergy"]["capitalShip"] then
            topUpEnergyValue = $GAME_PARAMETERS["fleetCapitalShipTopUpEnergyValue"]
            difficulty = $GAME_PARAMETERS["fleetCapitalShipTopUpChallengeDifficulty"]
            UserFleet::topUpCapitalShipAndResetTopUpChallenge(currentHour, username, topUpEnergyValue, difficulty)
            JSON.generate(GameLibrary::make200Answer(nil, currentHour, username))
        else
            JSON.generate(GameLibrary::makeErrorAnswer(403, "Your code is correct, please keep it (!), but you cannot submit it at this time. Your ship has too much energy in reserve."))
        end
    else
        JSON.generate(GameLibrary::makeErrorAnswer(403, "Your code is not a solution to the challenge"))
    end
end

get '/game/v1/:userkey/:mapid/capital-ship/create-battle-cruiser' do

    content_type 'application/json'

    userkey = params["userkey"]
    mapId = params["mapid"]

    currentHour = GameLibrary::hourCode()

    # ------------------------------------------------------
    # User Credentials and Map Validity Checks

    username = UserKeys::getUsernameFromUserkeyOrNull(userkey)

    if username.nil? then
        return JSON.generate(GameLibrary::makeErrorAnswer(401, "Invalid userkey"))
    end

    if MapUtils::getCurrentMap()["mapId"] != mapId then
        return JSON.generate(GameLibrary::makeErrorAnswer(404, "Map not found (mapId is incorrect or outdated)"))
    end

    # ------------------------------------------------------
    # Throttling

    if !Throttling::userRequestCanProceed(username) then
        return JSON.generate(GameLibrary::makeErrorAnswer(403, "You are playing too fast. Need to wait #{$GAME_PARAMETERS["serverThrottlingWaitingPeriodInSeconds"]} seconds between requests"))
    end

    Throttling::updateUserActionTime(username)

    # ------------------------------------------------------
    # User Fleet validation

    userFleet = UserFleet::getUserFleetDataOrNull(currentHour, username)

    if userFleet.nil? then
        return JSON.generate(GameLibrary::makeErrorAnswer(404, "You do not yet have a fleet for this hour. (You should initiate one.)"))
    end

    if !userFleet["ships"][0]["alive"] then
        return JSON.generate(GameLibrary::makeErrorAnswer(403, "Your capital ship for this hour is dead"))
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
        if !userFleet["ships"][0]["alive"] then
            return JSON.generate(GameLibrary::makeErrorAnswer(403, "Your capital ship doesn't have enough energy to complete the construction of a battle cruiser. You have #{userFleet["ships"][0]["energyLevel"]} but you need #{(battleCruiserBuildEnergyCost+battleCruiserInitialEnergyLevel)}"))
        end
    end

end

get '/game/v1/:userkey/:mapid/capital-ship/create-energy-carrier/:energyamount' do

    content_type 'application/json'

    userkey = params["userkey"]
    mapId = params["mapid"]

    energyamount = params["energyamount"].to_f

    currentHour = GameLibrary::hourCode()

    # ------------------------------------------------------
    # User Credentials and Map Validity Checks

    username = UserKeys::getUsernameFromUserkeyOrNull(userkey)

    if username.nil? then
        return JSON.generate(GameLibrary::makeErrorAnswer(401, "Invalid userkey"))
    end

    if MapUtils::getCurrentMap()["mapId"] != mapId then
        return JSON.generate(GameLibrary::makeErrorAnswer(404, "Map not found (mapId is incorrect or outdated)"))
    end

    # ------------------------------------------------------
    # Throttling

    if !Throttling::userRequestCanProceed(username) then
        return JSON.generate(GameLibrary::makeErrorAnswer(403, "You are playing too fast. Need to wait #{$GAME_PARAMETERS["serverThrottlingWaitingPeriodInSeconds"]} seconds between requests"))
    end

    Throttling::updateUserActionTime(username)

    # ------------------------------------------------------
    # User Fleet validation

    userFleet = UserFleet::getUserFleetDataOrNull(currentHour, username)

    if userFleet.nil? then
        return JSON.generate(GameLibrary::makeErrorAnswer(404, "You do not yet have a fleet for this hour. (You should initiate one.)"))
    end

    if !userFleet["ships"][0]["alive"] then
        return JSON.generate(GameLibrary::makeErrorAnswer(403, "Your capital ship for this hour is dead"))
    end

    if energyamount > $GAME_PARAMETERS["fleetShipsMaxEnergy"]["energyCarrier"] then
        return JSON.generate(GameLibrary::makeErrorAnswer(403, "You are creating a carrier with too much energy. Upper limit is #{$GAME_PARAMETERS["fleetShipsMaxEnergy"]["energyCarrier"]} units of energy."))    
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
        return JSON.generate(GameLibrary::makeErrorAnswer(403, "Your capital ship doesn't have enough energy to complete the construction of an energy carrier carrying #{carrierInitialEnergyLevel}. You have #{userFleet["ships"][0]["energyLevel"]} but you need #{(carrierBuildEnergyCost+carrierInitialEnergyLevel)}"))
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
    # User Credentials and Map Validity Checks

    username = UserKeys::getUsernameFromUserkeyOrNull(userkey)

    if username.nil? then
        return JSON.generate(GameLibrary::makeErrorAnswer(401, "Invalid userkey"))
    end

    if MapUtils::getCurrentMap()["mapId"] != mapId then
        return JSON.generate(GameLibrary::makeErrorAnswer(404, "Map not found (mapId is incorrect or outdated)"))
    end

    # ------------------------------------------------------
    # Throttling

    if !Throttling::userRequestCanProceed(username) then
        return JSON.generate(GameLibrary::makeErrorAnswer(403, "You are playing too fast. Need to wait #{$GAME_PARAMETERS["serverThrottlingWaitingPeriodInSeconds"]} seconds between requests"))
    end

    Throttling::updateUserActionTime(username)

    # ------------------------------------------------------
    # Map Validation

    map = MapUtils::getCurrentMap()    

    targetMapPoint = MapUtils::getPointForlabelAtMapOrNull(targetPointLabel, map)
    if targetMapPoint.nil? then
        return JSON.generate(GameLibrary::makeErrorAnswer(404, "The specified point doesn't exist"))
    end

    # ------------------------------------------------------
    # User Fleet validation

    userFleet = UserFleet::getUserFleetDataOrNull(currentHour, username)

    if userFleet.nil? then
        return JSON.generate(GameLibrary::makeErrorAnswer(404, "You do not yet have a fleet for this hour. (You should initiate one.)"))
    end

    # Need to check whether we own a ship of with that uuid, and retrieve it.
    ship = UserFleet::getShipPerUUIDOrNull(currentHour, username, shipuuid)
    if ship.nil? then
        return JSON.generate(GameLibrary::makeErrorAnswer(404, "Your fleet has no ship with this uuid"))
    end

    # Need to check whether the ship is alive ot not
    if !ship["alive"] then
        return JSON.generate(GameLibrary::makeErrorAnswer(403, "The ship is dead"))
    end    

    # In the current version of the game energy carriers need the capital ship to be alive 
    # in order to be controlled. Therfore we record whether or not the capital is alive.

    if ship["nomenclature"] == "energyCarrier" then
        if !userFleet["ships"][0]["alive"] then
            return JSON.generate(GameLibrary::makeErrorAnswer(403, "Your capital ship is dead. You cannot jump energy carriers in that case."))
        end 
    end

    sourceMapPoint = ship["location"]

    jec = Navigation::jumpEnergyCost(sourceMapPoint, targetMapPoint, ship["nomenclature"])

    # Need to check whether the ship has enough energy left to jump
    if ship["energyLevel"] < jec then
        return JSON.generate(GameLibrary::makeErrorAnswer(403, "The ship doesn't have enough energy for this jump. Available: #{ship["energyLevel"]}. Required: #{jec}"))
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
    # User Credentials and Map Validity Checks

    username = UserKeys::getUsernameFromUserkeyOrNull(userkey)

    if username.nil? then
        return JSON.generate(GameLibrary::makeErrorAnswer(401, "Invalid userkey"))
    end

    if MapUtils::getCurrentMap()["mapId"] != mapId then
        return JSON.generate(GameLibrary::makeErrorAnswer(404, "Map not found (mapId is incorrect or outdated)"))
    end

    # ------------------------------------------------------
    # Throttling

    if !Throttling::userRequestCanProceed(username) then
        return JSON.generate(GameLibrary::makeErrorAnswer(403, "You are playing too fast. Need to wait #{$GAME_PARAMETERS["serverThrottlingWaitingPeriodInSeconds"]} seconds between requests"))
    end

    Throttling::updateUserActionTime(username)

    # ------------------------------------------------------
    # User Fleet validation

    if ship1uuid==ship2uuid then
        return JSON.generate(GameLibrary::makeErrorAnswer(403, "You are transferring energy from a ship to itself."))
    end

    userFleet = UserFleet::getUserFleetDataOrNull(currentHour, username)

    if userFleet.nil? then
        return JSON.generate(GameLibrary::makeErrorAnswer(404, "You do not yet have a fleet for this hour. (You should initiate one.)"))
    end

    ship1 = UserFleet::getShipPerUUIDOrNull(currentHour, username, ship1uuid)
    ship2 = UserFleet::getShipPerUUIDOrNull(currentHour, username, ship2uuid)

    if ship1.nil? then
        return JSON.generate(GameLibrary::makeErrorAnswer(404, "Your fleet has no ship with uuid #{ship1uuid}"))
    end

    if ship2.nil? then
        return JSON.generate(GameLibrary::makeErrorAnswer(404, "Your fleet has no ship with uuid #{ship2uuid}"))
    end

    if !ship1["alive"] then
        return JSON.generate(GameLibrary::makeErrorAnswer(403, "The source ship, #{ship1uuid}, is dead"))
    end

    if !ship2["alive"] then
        return JSON.generate(GameLibrary::makeErrorAnswer(403, "The target ship, #{ship2uuid}, is dead"))
    end

    if ship2["location"]["label"] != ship1["location"]["label"] then
        return JSON.generate(GameLibrary::makeErrorAnswer(403, "You cannot transfer energy between the two ships, they are not at the same map location"))
    end

    if ship1["energyLevel"] == 0 then
        return JSON.generate(GameLibrary::makeErrorAnswer(403, "The source ship has no energy to transfer"))
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

    battleCruiserShipUUID = params["battlecruisershipuuid"]
    targetpointlabel = params["targetpointlabel"]

    currentHour = GameLibrary::hourCode()

    # ------------------------------------------------------
    # User Credentials and Map Validity Checks

    username = UserKeys::getUsernameFromUserkeyOrNull(userkey)

    if username.nil? then
        return JSON.generate(GameLibrary::makeErrorAnswer(401, "Invalid userkey"))
    end

    if MapUtils::getCurrentMap()["mapId"] != mapId then
        return JSON.generate(GameLibrary::makeErrorAnswer(404, "Map not found (mapId is incorrect or outdated)"))
    end    

    # ------------------------------------------------------
    # Throttling

    if !Throttling::userRequestCanProceed(username) then
        return JSON.generate(GameLibrary::makeErrorAnswer(403, "You are playing too fast. Need to wait #{$GAME_PARAMETERS["serverThrottlingWaitingPeriodInSeconds"]} seconds between requests"))
    end

    Throttling::updateUserActionTime(username)

    # ------------------------------------------------------
    # Map Validation

    map = MapUtils::getCurrentMap()

    targetMapPoint = MapUtils::getPointForlabelAtMapOrNull(targetpointlabel, map)
    if targetMapPoint.nil? then
        return JSON.generate(GameLibrary::makeErrorAnswer(404, "The specified point doesn't exist"))
    end

    # ------------------------------------------------------
    # User Fleet validation

    userFleet = UserFleet::getUserFleetDataOrNull(currentHour, username)

    if userFleet.nil? then
        return JSON.generate(GameLibrary::makeErrorAnswer(404, "You do not yet have a fleet for this hour. (You should initiate one.)"))
    end

    battleCruiser = UserFleet::getShipPerUUIDOrNull(currentHour, username, battleCruiserShipUUID)

    if battleCruiser.nil? then
        return JSON.generate(GameLibrary::makeErrorAnswer(404, "Your fleet has no ship with uuid #{battleCruiserShipUUID}"))
    end

    if !battleCruiser["alive"] then
        return JSON.generate(GameLibrary::makeErrorAnswer(403, "The battle cruiser is dead"))
    end

    # ------------------------------------------------------
    # At this point we can attempt shooting

    if battleCruiser["energyLevel"] < ( $GAME_PARAMETERS["fleetBattleCruiserBombBuildingCost"] + $GAME_PARAMETERS["fleetBattleCruiserBombNominalEnergy"] ) then
        return JSON.generate(GameLibrary::makeErrorAnswer(403, "Your cruiser doesn't have enough energy to complete the construction of a bomb"))   
    end

    battleCruiser["energyLevel"] = battleCruiser["energyLevel"] - ( $GAME_PARAMETERS["fleetBattleCruiserBombBuildingCost"] + $GAME_PARAMETERS["fleetBattleCruiserBombNominalEnergy"] )
    userFleet = UserFleet::insertOrUpdateShipAtFleet(userFleet, battleCruiser)

    # Ok, now time to do damage

    distanceToTargetPoint = MapUtils::distanceBetweenTwoMapPoints(battleCruiser["location"], targetMapPoint)
    bombEffectiveEnergy = BombsUtils::bombEffectiveEnergy($GAME_PARAMETERS["fleetBattleCruiserBombNominalEnergy"], distanceToTargetPoint)

    attackerBombDamageReport = []

    GameLibrary::userFleetsForHour(currentHour)
        .each{|otherPlayerUserFleet|
            UserFleet::userShipsWithinDisk(currentHour, otherPlayerUserFleet["username"], battleCruiser["location"], 0)
                .each{|targetShip|
                    otherPlayerUserFleet, targetShip, damageCausedForAttackerReport = UserFleet::registerShipTakingBombImpact(otherPlayerUserFleet, battleCruiser["location"], username, targetShip, bombEffectiveEnergy)
                    attackerBombDamageReport << damageCausedForAttackerReport
                    otherPlayerUserFleet = UserFleet::insertOrUpdateShipAtFleet(otherPlayerUserFleet, targetShip)
                }
            UserFleet::commitFleetToDisk(currentHour, otherPlayerUserFleet["username"], otherPlayerUserFleet)
        }

    JSON.generate(GameLibrary::make200Answer(attackerBombDamageReport, currentHour, username))
end

get '/game/v1/:userkey/:mapid/space-probe/:battlecruisershipuuid' do

    content_type 'application/json'

    userkey = params["userkey"]
    mapId = params["mapid"]

    battleCruiserShipUUID = params["battlecruisershipuuid"]

    currentHour = GameLibrary::hourCode()

    # ------------------------------------------------------
    # User Credentials and Map Validity Checks

    username = UserKeys::getUsernameFromUserkeyOrNull(userkey)

    if username.nil? then
        return JSON.generate(GameLibrary::makeErrorAnswer(401, "Invalid userkey"))
    end

    if MapUtils::getCurrentMap()["mapId"] != mapId then
        return JSON.generate(GameLibrary::makeErrorAnswer(404, "Map not found (mapId is incorrect or outdated)"))
    end    

    # ------------------------------------------------------
    # Throttling

    if !Throttling::userRequestCanProceed(username) then
        return JSON.generate(GameLibrary::makeErrorAnswer(403, "You are playing too fast. Need to wait #{$GAME_PARAMETERS["serverThrottlingWaitingPeriodInSeconds"]} seconds between requests"))
    end

    Throttling::updateUserActionTime(username)

    # ------------------------------------------------------
    # User Fleet validation

    userFleet = UserFleet::getUserFleetDataOrNull(currentHour, username)

    if userFleet.nil? then
        return JSON.generate(GameLibrary::makeErrorAnswer(404, "You do not yet have a fleet for this hour. (You should initiate one.)"))
    end

    battleCruiser = UserFleet::getShipPerUUIDOrNull(currentHour, username, battleCruiserShipUUID)

    if battleCruiser.nil? then
        return JSON.generate(GameLibrary::makeErrorAnswer(404, "Your fleet has no ship with uuid #{battleCruiserShipUUID}"))
    end

    if !battleCruiser["alive"] then
        return JSON.generate(GameLibrary::makeErrorAnswer(403, "The battle cruiser is dead"))
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
        GameLibrary::getHoursFolderPaths()
            .sort
            .map{|hoursFolderpath|
                currentHour = File.basename(hoursFolderpath)
                userFleetsOrdered = GameLibrary::userFleetsForHour(currentHour)
                    .sort{|f1, f2| f1["gameScore"] <=> f2["gameScore"] }
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
                        "#{userFleet["username"].ljust(20)} , game point: #{"%7.3f" % currentUserValue} , leaderboard score increment: #{score.round(3)}"
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
