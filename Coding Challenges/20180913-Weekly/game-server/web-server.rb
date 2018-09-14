
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

set :port, 14462
#set :public_folder, "path/to/www"

# -- --------------------------------------------------


LUCILLE_INSTANCE = ENV["COMPUTERLUCILLENAME"]

if LUCILLE_INSTANCE.nil? then
    puts "Error: Environment variable 'COMPUTERLUCILLENAME' is not defined."
    exit
end

DATA_FOLDER_PATH = "/Galaxy/DataBank/WeeklyCodingChallenges/20180913-Weekly/#{LUCILLE_INSTANCE}"

class GameLibrary

    # GameLibrary::hourCode()
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

    # GameLibrary::getMapAlphaDistance(map)
    def self.getMapAlphaDistance(map)
        distances = []
        points = map["points"].clone
        while points.size>1 do
            distances << GameLibrary::distanceBetweenTwoPoints(points[0], points[1])
            points.shift
        end
        # By now we have the distances from one point to another of the map in the order they are given in the map
        distances.inject(0, :+).to_f/distances.size
    end

    # GameLibrary::getMapAtHourFolderCreateIfNotExists(folderpath)
    def self.getMapAtHourFolderCreateIfNotExists(folderpath)
        mapfilepath = "#{folderpath}/map.json"
        if !File.exists?(mapfilepath) then
            mapCardinality = (20..40).to_a.sample
            map = {}
            map["mapId"] = SecureRandom.uuid
            map["timestamp"] = GameLibrary::hourCode()
            map["points"] = (1..mapCardinality).map{|indx|
                {
                    "label" => SecureRandom.hex(2),
                    "coordinates" => [ rand * 30, rand * 30 ].map{|c| c.round(2) }
                }
            }
            # By now we have a map without the energies set up
            alphaDistance = GameLibrary::getMapAlphaDistance(map)
            map["alpha-distance"] = alphaDistance
            map["points"] = map["points"].map{|point|
                point["energy"] = ( rand <= 0.8 ) ? ((alphaDistance**2)*( rand*0.6 + 0.6 )) : -(alphaDistance**2).to_f/2
                point
            }
            File.open(mapfilepath, "w"){ |f| f.puts(JSON.pretty_generate(map)) }
        end
        JSON.parse(IO.read(mapfilepath))
    end

    # GameLibrary::getCurrentMap()
    def self.getCurrentMap()
        folderpath = GameLibrary::getFolderpathForThisHourCreateIfNotExists()
        GameLibrary::getMapAtHourFolderCreateIfNotExists(folderpath)
    end

    # GameLibrary::getPointForlabelAtMapOrNull(label, map)
    def self.getPointForlabelAtMapOrNull(label, map)
        map["points"].each{|point|
            return point if point["label"]==label
        }
        nil
    end

    # GameLibrary::distanceBetweenTwoPoints(point1, point2)
    def self.distanceBetweenTwoPoints(point1, point2)
        dx = point1["coordinates"][0] - point2["coordinates"][0]
        dy = point1["coordinates"][1] - point2["coordinates"][1]
        Math.sqrt( (dx**2) + (dy**2) )
    end

    # GameLibrary::computePointsSequenceValue(points)
    def self.computePointsSequenceValue(points)
        return 0 if points.size==0
        lenghtAccumulation = 0
        energyLevel = points[0]["energy"]
        loop {
            #puts JSON.generate([points, lenghtAccumulation, energyLevel])
            if points.size==1 then
                return lenghtAccumulation
            end
            point1 = points.shift
            point2 = points[0]
            distanceToNext = GameLibrary::distanceBetweenTwoPoints(point1, point2)
            if distanceToNext**2 < energyLevel then
                lenghtAccumulation = lenghtAccumulation + distanceToNext
                energyLevel = energyLevel - distanceToNext**2
                energyLevel = energyLevel + point2["energy"]
            else
                return lenghtAccumulation
            end
        }
    end

    # GameLibrary::pathValueAgainstMap(pathAsString, map)
    def self.pathValueAgainstMap(pathAsString, map)
        points = pathAsString.split(",").map{|l| l.strip }.map{|label| GameLibrary::getPointForlabelAtMapOrNull(label, map).compact }
        GameLibrary::computePointsSequenceValue(points)
    end

    # GameLibrary::existingUserSubmissionForThisHourOrNull(username)
    def self.existingUserSubmissionForThisHourOrNull(username)
        usernamex = Digest::SHA1.hexdigest(username)[0,8]
        usernameSubmissionFilepath = "#{GameLibrary::getFolderpathForThisHourCreateIfNotExists()}/#{usernamex}.json"
        if File.exists?(usernameSubmissionFilepath) then
            JSON.parse(IO.read(usernameSubmissionFilepath))
        else
            nil
        end
    end

    # GameLibrary::commitUserDataToDiskForThisHour(username, mapId, path)
    def self.commitUserDataToDiskForThisHour(username, mapId, path)
        usernamex = Digest::SHA1.hexdigest(username)[0,8]
        usernameSubmissionFilepath = "#{GameLibrary::getFolderpathForThisHourCreateIfNotExists()}/#{usernamex}.json"
        data = {
            "username" => username,
            "mapid" => mapId,
            "path" => path
        }
        File.open(usernameSubmissionFilepath, "w"){|f| f.puts(JSON.pretty_generate(data)) }
        data
    end

    # GameLibrary::getHoursFolderPaths()
    def self.getHoursFolderPaths()
        Dir.entries(DATA_FOLDER_PATH)
            .select{|filename| filename[0,1]!="." }
            .map{|filename| "#{DATA_FOLDER_PATH}/#{filename}" }
    end

    # GameLibrary::getUserSubmissionFilepathsFor(hoursFolderpath)
    def self.getUserSubmissionFilepathsFor(hoursFolderpath)
        Dir.entries(hoursFolderpath)
            .select{|filename| filename[0,1]!="." }
            .select{|filename| filename != "map.json" }
            .map{|filename| "#{hoursFolderpath}/#{filename}" }
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
        "Game server for 20180913-Weekly, running at #{LUCILLE_INSTANCE}",
        "See https://github.com/guardian/techtime/tree/master/Coding%20Challenges/20180913-Weekly for details."
    ].join("\n")
end

get '/game/v1/map/:timestamp' do
    content_type 'application/json'
    hourcode = params['timestamp']
    if /^\d\d\d\d-\d\d-\d\d-\d\d$/.match(hourcode) then
        folderpath = "#{DATA_FOLDER_PATH}/#{hourcode}"
        if !File.exists?(folderpath) then
            status 404
            ""
        else
            mapfilepath = "#{folderpath}/map.json"
            map = JSON.parse(IO.read(mapfilepath))
            answer = {}
            answer["map"] = map
            JSON.generate(map)
        end
    else
        status 403
        ""
    end
end

get '/game/v1/map' do
    content_type 'application/json'
    JSON.pretty_generate(GameLibrary::getCurrentMap())
end

get '/game/v1/submit/:username/:mapid/:path' do
    
    content_type 'application/json'

    username = params['username']
    mapId    = params['mapid']
    path     = params['path']

    currentMap = GameLibrary::getCurrentMap()

    if currentMap["mapId"] != mapId then
        status 401
        return "Invalid map identifier (are you using an outdated one ?)\n"
    end

    currentMapPointLabels = currentMap["points"].map{|point| point["label"] }

    path_corrected = path.split(",").select{|label| currentMapPointLabels.include?(label) }.uniq.join(",") 
        # Remove inexistent labels from the user submission, and
        # enforce that labels should be present once.

    if path != path_corrected then
        path = path_corrected
        # This means that we rewrite user's submissions to be valid instead of rejecting them outright. Design choice. 
    end

    puts "#{username}: #{path}"

    existingUserSubmissionOrNull = GameLibrary::existingUserSubmissionForThisHourOrNull(username)
    if existingUserSubmissionOrNull.nil? then
        data = GameLibrary::commitUserDataToDiskForThisHour(username, mapId, path)
        data["pathValue"] = GameLibrary::pathValueAgainstMap(path, currentMap)
        JSON.pretty_generate(data)
    else
        existingUserSubmission = existingUserSubmissionOrNull
        existingPathAsString = existingUserSubmission["path"]
        proposedPathAsString = path
        existingPathValue = GameLibrary::pathValueAgainstMap(existingPathAsString, currentMap)
        proposedPathValue = GameLibrary::pathValueAgainstMap(proposedPathAsString, currentMap)
        if proposedPathValue >= existingPathValue then
            data = GameLibrary::commitUserDataToDiskForThisHour(username, mapId, proposedPathAsString)
            data["pathValue"] = proposedPathValue
            JSON.pretty_generate(data)
        else
            status 409 # Conflict
            "You already have a better (or equivalent) path in store: #{existingUserSubmission}\n"
        end
    end
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
            .map{|hoursFolderpath|
                map = JSON.parse(IO.read("#{hoursFolderpath}/map.json"))
                userSubmissionOrdered = GameLibrary::getUserSubmissionFilepathsFor(hoursFolderpath)
                    .map{|filepath|
                        JSON.parse(IO.read(filepath))
                    }
                    .sort{|u1,u2|
                        GameLibrary::pathValueAgainstMap(u1["path"], map) <=> GameLibrary::pathValueAgainstMap(u2["path"], map)
                    }
                    .reverse
                score = 0.1/0.7
                lastlength = nil
                [
                    "",
                    File.basename(hoursFolderpath),
                    userSubmissionOrdered.map{|u|
                        currentUserValue = GameLibrary::pathValueAgainstMap(u["path"], map).round(3)
                        if currentUserValue != lastlength then
                            score = score*0.7 
                        end
                        lastlength = currentUserValue
                        users = addScoreToUserLambda.call(users, u["username"], score)
                        "#{u["username"].ljust(20)} , value ( length ): #{"%7.3f" % currentUserValue} , score: #{score.round(3)}"
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
