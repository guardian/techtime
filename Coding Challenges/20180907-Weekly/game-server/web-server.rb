
# encoding: UTF-8

# ------------------------------------------------------------
# utils

require 'sinatra'
# http://www.sinatrarb.com/intro.html

require 'securerandom'
# SecureRandom.hex    #=> "eb693ec8252cd630102fd0d0fb7c3485"
# SecureRandom.hex(2) #=> "eb69"
# SecureRandom.uuid   #=> "2d931510-d99f-494a-8c67-87feb05e1594"

require 'json'

require 'find'

# --  --------------------------------------------------

set :port, 14361
#set :public_folder, "path/to/www"

# -- --------------------------------------------------


LUCILLE_INSTANCE = ENV["COMPUTERLUCILLENAME"]

if LUCILLE_INSTANCE.nil? then
    puts "Error: Environment variable 'COMPUTERLUCILLENAME' is not defined."
    exit
end

DATA_FOLDER_PATH = "/Galaxy/DataBank/WeeklyCodingChallenges/20180907-Weekly/#{LUCILLE_INSTANCE}"

class GameLibrary

    def self.hourCode()
        Time.new.strftime("%Y-%m-%d-%H")
    end

    # GameLibrary::getFolderpathForThisHourCreateIfNotExists(): folderpath
    def self.getFolderpathForThisHourCreateIfNotExists()
        folderpath = "#{DATA_FOLDER_PATH}/#{GameLibrary::hourCode()}"
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
            map["points"] = (1..12).map{|indx|
                {
                    "label" => SecureRandom.hex(2),
                    "coordinates" => [ rand * 100 - 50, rand * 100 - 50 ].map{|c| c.round(2) }
                }
            }
            File.open(mapfilepath, "w"){ |f| f.puts(JSON.pretty_generate(map)) }
        end
        JSON.parse(IO.read(mapfilepath))
    end

    # GameLibrary::getCurrentGame()
    def self.getCurrentGame()
        folderpath = GameLibrary::getFolderpathForThisHourCreateIfNotExists()
        GameLibrary::getMapAtHourFolderCreateIfNotExists(folderpath)
    end

end

# -- --------------------------------------------------
# Route

not_found do
  '404'
end

get '/' do
    content_type 'text/plain'
    [
        "Game server for 20180907-Weekly, running at #{LUCILLE_INSTANCE}",
        "See https://github.com/guardian/techtime/tree/master/Coding%20Challenges/20180907-Weekly for details."
    ].join("\n")
end

get '/game/v1/map' do
    content_type 'application/json'
    JSON.pretty_generate(GameLibrary::getCurrentGame())
end

