
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

# --  --------------------------------------------------

require_relative "library/BombsUtils.rb"
require_relative "library/MapUtils.rb"
require_relative "library/Navigation.rb"
require_relative "library/UserKeys.rb"
require_relative "library/UserFleet.rb"

# --  --------------------------------------------------

set :port, 14561
#set :public_folder, "path/to/www"

LUCILLE_INSTANCE = ENV["COMPUTERLUCILLENAME"]

if LUCILLE_INSTANCE.nil? then
    puts "Error: Environment variable 'COMPUTERLUCILLENAME' is not defined."
    exit
end

GAME_DATA_FOLDERPATH = "/Galaxy/DataBank/WeeklyCodingChallenges/20180920-Weekly/#{LUCILLE_INSTANCE}"
GAME_PARAMETERS_FILEPATH = File.dirname(__FILE__) + "/game-parameters.json"
$GAME_PARAMETERS = JSON.parse(IO.read(GAME_PARAMETERS_FILEPATH))

# -- --------------------------------------------------
# nslog

=begin

mapPoint = {
    "label" => "12345678",
    "coordinates" => [ 123 , 34.98]
}
puts UserFleet::spawnBattleCruiser(mapPoint, 12)

exit 

=end

# -- --------------------------------------------------

class GameLibrary

    # GameLibrary::hourCode()
    def self.hourCode()
        Time.new.strftime("%Y-%m-%d-%H")
    end

    # GameLibrary::getHoursFolderPaths()
    def self.getHoursFolderPaths()
        Dir.entries(GAME_DATA_FOLDERPATH)
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

        folderpath = "#{GAME_DATA_FOLDERPATH}/Timeline/#{GameLibrary::hourCode()}"
        if !File.exists?(folderpath) then
            FileUtils.mkpath folderpath
        end

        mapfilepath = "#{folderpath}/map.json"
        return if File.exists?(mapfilepath)

        # ---------------------------------------
        # The Map
        map = {}
        map["mapId"] = SecureRandom.uuid
        map["timestamp"] = GameLibrary::hourCode()
        map["points"] = (1..$GAME_PARAMETERS["map:jump-points:cardinality"]).map{|indx|
            {
                "label" => SecureRandom.hex(4),
                "coordinates" => [ rand * $GAME_PARAMETERS["map:size"], rand * $GAME_PARAMETERS["map:size"] ].map{|c| c.round(2) }
            }
        }
        File.open(mapfilepath, "w"){ |f| f.puts(JSON.pretty_generate(map)) }

        # ---------------------------------------
        # Game Parameters
        FileUtils.cp(GAME_PARAMETERS_FILEPATH, "#{folderpath}/game-parameters.json")

        # ---------------------------------------
        # The BBC Fleet

        folderpath

    end

    # GameLibrary::doUserFleetPointIncreaseForShipDestroyed(currentHour, username, nomenclature)
    def self.doUserFleetPointIncreaseForShipDestroyed(currentHour, username, nomenclature)
        userFleet = UserFleet::getUserFleetDataOrNull(currentHour, username)
        userFleet = ScoringUtils::userFleetPointIncreaseForShipDestroyed(userFleet, nomenclature)
        UserFleet::commitFleetToDisk(currentHour, username, fleet)
    end

end

# -- --------------------------------------------------
# Route

=begin

    HTTP error codes:
        401 Unauthorized
        403 Forbidden
        404 Not Found

=end

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
        status 403
        return "Usernames cannot contain a colon (character ':')\n"
    end

    userKeysData = UserKeys::getUserKeysData()
    if userKeysData.any?{|record| record[0]==username } then
        status 403
        "There has already been a userkey issued for this username. If you think this is a mistake or you have forgotten your userkey, please contact Pascal.\n"
    else
        userkey = SecureRandom.hex(4)
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

get '/game/v1/:username/:userkey/:mapid/capital-ship/init' do
    username = params["username"]
    userkey = params["userkey"]
    mapId = params["mapid"]

    currentHour = GameLibrary::hourCode()

    # ------------------------------------------------------
    # User Credentials and Map Validity Checks

    if !UserKeys::validateUserCredentials(username, userkey) then
        status 401
        return "401: Invalid credentials\n"
    end

    if MapUtils::getCurrentMap()["mapId"] != mapId then
        status 404
        return "404: Map not found (mapId is incorrect or outdated)\n"
    end

    # ------------------------------------------------------
    # User Fleet validation

    if UserFleet::getUserFleetDataOrNull(currentHour, username) then
        status 403
        return "403: You cannot init a Capital Ship, you already have one for this hour\n"
    end

    # ------------------------------------------------------

    content_type 'application/json'

    mapPoint = MapUtils::getCurrentMap()["points"].sample
    capitalShipInitialEnergy = $GAME_PARAMETERS["fleet:capital-ship:initial-energy-level"]
    topUpChallengeDifficulty = $GAME_PARAMETERS["fleet:capital-ship:top-up-challenge-difficulty"]
    userFleet = UserFleet::spawnUserFleet(username, mapPoint, capitalShipInitialEnergy, topUpChallengeDifficulty)

    UserFleet::commitFleetToDisk(currentHour, username, userFleet)

    JSON.generate(userFleet)
end

get '/game/v1/:username/:userkey/:mapid/capital-ship/top-up/:code' do
    username = params["username"]
    userkey = params["userkey"]
    mapId = params["mapid"]
    code = params["code"]

    currentHour = GameLibrary::hourCode()

    # ------------------------------------------------------
    # User Credentials and Map Validity Checks

    if !UserKeys::validateUserCredentials(username, userkey) then
        status 401
        return "401: Invalid credentials\n"
    end

    if MapUtils::getCurrentMap()["mapId"] != mapId then
        status 404
        return "404: Map not found (mapId is incorrect or outdated)\n"
    end

    # ------------------------------------------------------
    # User Fleet validation

    userFleet = UserFleet::getUserFleetDataOrNull(currentHour, username)

    if userFleet.nil? then
        status 404
        return "404: You do not yet have a fleet for this hour. (You should initiate one.)\n"
    end

    if !userFleet["ship-inventory"]["capital"]["alive"] then
        status 403
        return "403: Your capital ship for this hour is dead.\n"
    end

    # ------------------------------------------------------    

    content_type 'application/json'

    if UserFleet::validateTopUpCode(currentHour, username, code) then
        # We need: (1) top up the value, (2) issue a new challenge 
        topUpEnergyValue = $GAME_PARAMETERS["fleet:capital-ship:top-up-energy-value"]
        difficulty = $GAME_PARAMETERS["fleet:capital-ship:top-up-challenge-difficulty"]
        UserFleet::topUpCapitalShipAndResetTopUpChallenge(currentHour, username, topUpEnergyValue)
        JSON.generate([true])
    else
        status 403
        return "403: Your code is not a solution to the challenge.\n"
    end
end

get '/game/v1/:username/:userkey/:mapid/capital-ship/create-battle-cruiser' do
    username = params["username"]
    userkey = params["userkey"]
    mapId = params["mapid"]

    currentHour = GameLibrary::hourCode()

    # ------------------------------------------------------
    # User Credentials and Map Validity Checks

    if !UserKeys::validateUserCredentials(username, userkey) then
        status 401
        return "401: Invalid credentials\n"
    end

    if MapUtils::getCurrentMap()["mapId"] != mapId then
        status 404
        return "404: Map not found (mapId is incorrect or outdated)\n"
    end

    # ------------------------------------------------------
    # User Fleet validation

    userFleet = UserFleet::getUserFleetDataOrNull(currentHour, username)

    if userFleet.nil? then
        status 404
        return "404: You do not yet have a fleet for this hour. (You should initiate one.)\n"
    end

    if !userFleet["ship-inventory"]["capital"]["alive"] then
        status 403
        return "403: Your capital ship for this hour is dead.\n"
    end

    # ------------------------------------------------------ 

    content_type 'application/json'

    battleCruiserBuildEnergyCost = $GAME_PARAMETERS["fleet:battle-cruiser:build-energy-cost"]
    battleCruiserInitialEnergyLevel = $GAME_PARAMETERS["fleet:battle-cruiser:initial-energy-level"]

    userFleet = UserFleet::getUserFleetDataOrNull(currentHour, username)
    capitalShipCanPerformBattleShipCreation = userFleet["ship-inventory"]["capital"]["energy-level"] >= ( battleCruiserBuildEnergyCost + battleCruiserInitialEnergyLevel )
    if capitalShipCanPerformBattleShipCreation then
        userFleet["ship-inventory"]["capital"]["energy-level"] = userFleet["ship-inventory"]["capital"]["energy-level"] - ( battleCruiserBuildEnergyCost + battleCruiserInitialEnergyLevel )
        mapPoint = MapUtils::getCurrentMap()["points"].sample
        battleCruiser = UserFleet::spawnBattleCruiser(mapPoint, battleCruiserInitialEnergyLevel)
        userFleet["ship-inventory"]["battle-cruisers"] << battleCruiser
        UserFleet::commitFleetToDisk(currentHour, username, userFleet)
        JSON.generate(battleCruiser)
    else
        status 403
        "403: Your capital ship doesn't have enough energy to complete the construction of a battle cruiser. You have #{userFleet["ship-inventory"]["capital"]["energy-level"]} but you need #{(battleCruiserBuildEnergyCost+battleCruiserInitialEnergyLevel)}\n"
    end

end

get '/game/v1/:username/:userkey/:mapid/capital-ship/create-energy-carrier/:energyamount' do
    username = params["username"]
    userkey = params["userkey"]
    mapId = params["mapid"]

    energyamount = params["energyamount"]

    currentHour = GameLibrary::hourCode()

    # ------------------------------------------------------
    # User Credentials and Map Validity Checks

    if !UserKeys::validateUserCredentials(username, userkey) then
        status 401
        return "401: Invalid credentials\n"
    end

    if MapUtils::getCurrentMap()["mapId"] != mapId then
        status 404
        return "404: Map not found (mapId is incorrect or outdated)\n"
    end

    # ------------------------------------------------------
    # User Fleet validation

    userFleet = UserFleet::getUserFleetDataOrNull(currentHour, username)

    if userFleet.nil? then
        status 404
        return "404: You do not yet have a fleet for this hour. (You should initiate one.)\n"
    end

    if !userFleet["ship-inventory"]["capital"]["alive"] then
        status 403
        return "403: Your capital ship for this hour is dead.\n"
    end

    # ------------------------------------------------------ 

    content_type 'application/json'

    carrierBuildEnergyCost = $GAME_PARAMETERS["fleet:energy-carrier:build-energy-cost"]
    carrierInitialEnergyLevel = energyamount 

    userFleet = UserFleet::getUserFleetDataOrNull(currentHour, username)
    capitalShipCanPerformCarrierCreation = userFleet["ship-inventory"]["capital"]["energy-level"] >= ( carrierBuildEnergyCost + carrierInitialEnergyLevel )
    if capitalShipCanPerformCarrierCreation then
        userFleet["ship-inventory"]["capital"]["energy-level"] = userFleet["ship-inventory"]["capital"]["energy-level"] - ( carrierBuildEnergyCost + carrierInitialEnergyLevel )
        mapPoint = MapUtils::getCurrentMap()["points"].sample
        energyCarrier = UserFleet::spawnEnergyCarrier(mapPoint, carrierInitialEnergyLevel)
        userFleet["ship-inventory"]["energy-carriers"] << energyCarrier
        UserFleet::commitFleetToDisk(currentHour, username, userFleet)
        JSON.generate(energyCarrier)
    else
        status 403
        "403: Your capital ship doesn't have enough energy to complete the construction of an energy carrier carrying #{carrierInitialEnergyLevel}. You have #{userFleet["ship-inventory"]["capital"]["energy-level"]} but you need #{(carrierBuildEnergyCost+carrierInitialEnergyLevel)}\n"
    end

    "{}"
end

get '/game/v1/:username/:userkey/:mapid/jump/:shipuuid/:targetpointlabel' do
    username = params["username"]
    userkey = params["userkey"]
    mapId = params["mapid"]

    shipuuid = params["shipuuid"]
    targetPointLabel = params["targetpointlabel"]

    currentHour = GameLibrary::hourCode()

    # ------------------------------------------------------
    # User Credentials and Map Validity Checks

    if !UserKeys::validateUserCredentials(username, userkey) then
        status 401
        return "401: Invalid credentials\n"
    end

    if MapUtils::getCurrentMap()["mapId"] != mapId then
        status 404
        return "404: Map not found (mapId is incorrect or outdated)\n"
    end

    # ------------------------------------------------------
    # Map Validation

    targetMapPoint = MapUtils::getPointForlabelAtMapOrNull(label, map)
    if targetMapPoint.nil? then
        status 404
        return "404: The specified point doesn't exist\n"
    end

    # ------------------------------------------------------
    # User Fleet validation

    userFleet = UserFleet::getUserFleetDataOrNull(currentHour, username)

    if userFleet.nil? then
        status 404
        return "404: You do not yet have a fleet for this hour. (You should initiate one.)\n"
    end

    # Need to check whether we own a ship of with that uuid, and retrieve it.
    ship = UserFleet::getShipPerUUIDOrNull(currentHour, username, uuid)
    if ship.nil? then
        status 404
        return "404: Your fleet has no ship with this uuid.\n"
    end

    # Need to check whether the ship is alive ot not
    if !ship["alive"] then
        status 403
        return "403: The ship is dead.\n"
    end    

    # In the current version of the game energy carriers need the capital ship to be alive 
    # in order to be controlled. Therfore we record whether or not the capital is alive.

    if ship["nomenclature"] == "energy-carrier" then
        if !userFleet["ship-inventory"]["capital"]["alive"] then
            status 403
            return "403: Your capital ship is dead. You cannot jump energy carriers in that case.\n"
        end 
    end

    sourceMapPoint = ship["location"]

    jec = Navigation::jumpEnergyCost(sourceMapPoint, targetMapPoint, ship["nomenclature"])

    # Need to check whether the ship has enough energy left to jump
    if !ship["energy-level"] < jec then
        status 403
        return "403: The ship doesn't have enough energy for this jump. Available: #{ship["energy-level"]}. Required: #{jec}.\n"
    end    

    # ------------------------------------------------------
    
    # Now performing the jump
    ship["location"] = targetMapPoint
    ship["energy-level"] = ship["energy-level"] - jec

    userFleet = UserFleet::insertOrUpdateShipAtFleet(userFleet, ship)
    UserFleet::commitFleetToDisk(currentHour, username, userFleet)

    content_type 'application/json'
    JSON.pretty_generate(userFleet)

end

get '/game/v1/:username/:userkey/:mapid/energy-transfer-type1/:energycarriershipuuid/:energylevel' do

    username = params["username"]
    userkey = params["userkey"]
    mapId = params["mapid"]

    energyCarrierShipUUID = params["energycarriershipuuid"]
    energyLevel = params["energylevel"]

    currentHour = GameLibrary::hourCode()

    # ------------------------------------------------------
    # User Credentials and Map Validity Checks

    if !UserKeys::validateUserCredentials(username, userkey) then
        status 401
        return "401: Invalid credentials\n"
    end

    if MapUtils::getCurrentMap()["mapId"] != mapId then
        status 404
        return "404: Map not found (mapId is incorrect or outdated)\n"
    end

    # ------------------------------------------------------
    # User Fleet validation

    userFleet = UserFleet::getUserFleetDataOrNull(currentHour, username)

    if userFleet.nil? then
        status 404
        return "404: You do not yet have a fleet for this hour. (You should initiate one.)\n"
    end

    energyCarrier = UserFleet::getShipPerUUIDOrNull(currentHour, username, energyCarrierShipUUID)
    capital = userFleet["ship-inventory"]["capital"]

    if energyCarrier.nil? then
        status 404
        return "404: Your fleet has no ship with uuid #{energyCarrierShipUUID}.\n"
    end

    if !energyCarrier["alive"] then
        status 403
        return "403: The energy carrier is dead.\n"
    end

    if !capital["alive"] then
        status 403
        return "403: Your capital ship is dead.\n"
    end

    if capital["location"]["label"] != energyCarrier["location"]["label"] then
        status 403
        return "403: You cannot transfer energy between the two ships, they are not at the same map location.\n"        
    end

    if capital["energy-level"] < energyLevel then
        status 403
        return "403: Your capital ship doesn't have enough energy for this transfer.\n"
    end    

    # ------------------------------------------------------

    capital["energy-level"] = capital["energy-level"] - energyLevel
    energyCarrier["energy-level"] = energyCarrier["energy-level"] + energyLevel

    userFleet["ship-inventory"]["capital"] = capital
    userFleet = UserFleet::insertOrUpdateShipAtFleet(userFleet, energyCarrier)
    UserFleet::commitFleetToDisk(currentHour, username, userFleet)

    JSON.pretty_generate([ energyCarrier, capital ])

end

get '/game/v1/:username/:userkey/:mapid/energy-transfer-type2/:energycarriershipuuid/:battlecruisershipuuid' do

    username = params["username"]
    userkey = params["userkey"]
    mapId = params["mapid"]

    energyCarrierShipUUID = params["energycarriershipuuid"]
    battleCruiserShipUUID = params["battlecruisershipuuid"]

    currentHour = GameLibrary::hourCode()

    # ------------------------------------------------------
    # User Credentials and Map Validity Checks

    if !UserKeys::validateUserCredentials(username, userkey) then
        status 401
        return "401: Invalid credentials\n"
    end

    if MapUtils::getCurrentMap()["mapId"] != mapId then
        status 404
        return "404: Map not found (mapId is incorrect or outdated)\n"
    end

    # ------------------------------------------------------
    # User Fleet validation

    userFleet = UserFleet::getUserFleetDataOrNull(currentHour, username)

    if userFleet.nil? then
        status 404
        return "404: You do not yet have a fleet for this hour. (You should initiate one.)\n"
    end

    energyCarrier = UserFleet::getShipPerUUIDOrNull(currentHour, username, energyCarrierShipUUID)
    battleCruiser = UserFleet::getShipPerUUIDOrNull(currentHour, username, battleCruiserShipUUID)

    if energyCarrier.nil? then
        status 404
        return "404: Your fleet has no ship with uuid #{energyCarrierShipUUID}.\n"
    end

    if battleCruiser.nil? then
        status 404
        return "404: Your fleet has no ship with uuid #{battleCruiserShipUUID}.\n"
    end

    if !energyCarrier["alive"] then
        status 403
        return "403: The energy carrier is dead.\n"
    end

    if !battleCruiser["alive"] then
        status 403
        return "403: The battle cruiser is dead.\n"
    end

    if battleCruiser["location"]["label"] != energyCarrier["location"]["label"] then
        status 403
        return "403: You cannot transfer energy between the two ships, they are not at the same map location.\n"        
    end

    if energyCarrier["energy-level"] == 0 then
        status 403
        return "403: The energy carrier is empty.\n"
    end    

    # ------------------------------------------------------

    battleCruiser["energy-level"] = battleCruiser["energy-level"] + energyCarrier["energy-level"]
    energyCarrier["energy-level"] = 0

    userFleet = UserFleet::insertOrUpdateShipAtFleet(userFleet, battleCruiser)
    userFleet = UserFleet::insertOrUpdateShipAtFleet(userFleet, energyCarrier)
    UserFleet::commitFleetToDisk(currentHour, username, userFleet)

    JSON.pretty_generate([ energyCarrier, battleCruiser ])

end

get '/game/v1/:username/:userkey/:mapid/bomb/:battlecruisershipuuid/:targetpointlabel' do
    status 404
    return "404: Not implemented yet.\n"

    # GameLibrary::doUserFleetPointIncreaseForShipDestroyed(currentHour, username, nomenclature)
end

