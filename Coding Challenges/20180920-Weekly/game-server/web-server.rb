
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

# --  --------------------------------------------------

require_relative "library/MapUtils.rb"
require_relative "library/UserKeys.rb"

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
                    "label" => SecureRandom.hex(3),
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

get '/game/v1/map' do
    content_type 'application/json'
    JSON.generate(MapUtils::getCurrentMap())
end

get '/game/v1/parameters' do
    content_type 'application/json'
    JSON.pretty_generate($GAME_PARAMETERS)
end

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

get '/game/v1/:username/:userkey/capital-ship/top-up/:code' do
    content_type 'application/json'
    username = params["username"]
    userkey = params["userkey"]
    code = params["code"]

    if !UserKeys::validateUserCredentials(username, userkey) then
        status 401
        return "401: Invalid credentials\n"
    end

    "[]"
end
