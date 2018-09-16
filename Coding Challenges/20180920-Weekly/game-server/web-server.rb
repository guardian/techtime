
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

require_relative "library/MapUtils.rb"
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

    # GameLibrary::getFolderpathForThisHourCreateIfNotExists(): folderpath
    def self.getFolderpathForThisHourCreateIfNotExists()
        folderpath = "#{GAME_DATA_FOLDERPATH}/Timeline/#{GameLibrary::hourCode()}"
        if !File.exists?(folderpath) then
            FileUtils.mkpath folderpath
        end
        folderpath
    end

    # GameLibrary::getMapAtHourFolderCreateIfNotExists(folderpath)
    def self.getMapAtHourFolderCreateIfNotExists(folderpath)
        mapfilepath = "#{folderpath}/map.json"
        if !File.exists?(mapfilepath) then
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
        end
        JSON.parse(IO.read(mapfilepath))
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

    if userFleet.nil then
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
        UserFleet::topUpEnergyValue(currentHour, username, topUpEnergyValue)
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

    if userFleet.nil then
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
        "403: Your capital ship doesn't have enough energy to complete the construction of a battle cruiser. You have #{userFleet["ship-inventory"]["capital"]["energy-level"]} but yuo need #{(battleCruiserBuildEnergyCost+battleCruiserInitialEnergyLevel)}\n"
    end

end

get '/game/v1/:username/:userkey/:mapid/capital-ship/create-energy-carrier/:energyamount' do
    username = params["username"]
    userkey = params["userkey"]
    mapId = params["mapid"]

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
    # Game Mechanics Validation

    # ------------------------------------------------------

    content_type 'application/json'

    "{}"
end

get '/game/v1/:username/:userkey/:mapid/jump/:shipuuid/:targetpointlabel' do
    username = params["username"]
    userkey = params["userkey"]
    mapId = params["mapid"]

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
    # Game Mechanics Validation

    # ------------------------------------------------------

    content_type 'application/json'

    "{}"
end


