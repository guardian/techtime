
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

set :port, 14361
#set :public_folder, "path/to/www"

# -- --------------------------------------------------


LUCILLE_INSTANCE = ENV["COMPUTERLUCILLENAME"]

if LUCILLE_INSTANCE.nil? then
    puts "Error: Environment variable 'COMPUTERLUCILLENAME' is not defined."
    exit
end

DATA_FOLDER_PATH = "/Galaxy/DataBank/WeeklyCodingChallenges/20180907-Weekly/#{LUCILLE_INSTANCE}"

PATH_LENGTH = 13

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

    # GameLibrary::getMapAtHourFolderCreateIfNotExists(folderpath)
    def self.getMapAtHourFolderCreateIfNotExists(folderpath)
        mapfilepath = "#{folderpath}/map.json"
        if !File.exists?(mapfilepath) then
            map = {}
            map["mapId"] = SecureRandom.uuid
            map["timestamp"] = GameLibrary::hourCode()
            map["points"] = (1..PATH_LENGTH).map{|indx|
                {
                    "label" => SecureRandom.hex(2),
                    "coordinates" => [ rand * 100 - 50, rand * 100 - 50 ].map{|c| c.round(2) }
                }
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

    # GameLibrary::userSubmittedPathIsValidForMap(pathAsString, map)
    def self.userSubmittedPathIsValidForMap(pathAsString, map)
        mapLabels = map["points"].map{|point| point["label"] }
        userLabels = pathAsString.split(",").map{|l| l.strip }
        ( mapLabels - userLabels ).size==0 and ( userLabels - mapLabels ).size==0
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

    # GameLibrary::computePointsSequenceLengthCore(lenghtAccumulation, pointsDone, pointsLeft)
    def self.computePointsSequenceLengthCore(lenghtAccumulation, pointsDone, pointsLeft)
        if pointsLeft.size==0 then
            return lenghtAccumulation
        end
        if pointsDone.size==0 then
            nextPoint = pointsLeft.shift
            return GameLibrary::computePointsSequenceLengthCore(lenghtAccumulation, [ nextPoint ], pointsLeft)
        end
        point1 = pointsDone.last
        point2 = pointsLeft.shift
        lenghtAccumulation = lenghtAccumulation + GameLibrary::distanceBetweenTwoPoints(point1, point2)
        GameLibrary::computePointsSequenceLengthCore(lenghtAccumulation, pointsDone + [point2], pointsLeft)
    end

    # GameLibrary::computePointsSequenceLengthInterface(points)
    def self.computePointsSequenceLengthInterface(points)
        GameLibrary::computePointsSequenceLengthCore(0, [], points)
    end

    # GameLibrary::pathLengthAgainstMap(pathAsString, map)
    def self.pathLengthAgainstMap(pathAsString, map)
        points = pathAsString.split(",").map{|l| l.strip }.map{|label| GameLibrary::getPointForlabelAtMapOrNull(label, map) }
        GameLibrary::computePointsSequenceLengthInterface(points)
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
        "Game server for 20180907-Weekly, running at #{LUCILLE_INSTANCE}",
        "See https://github.com/guardian/techtime/tree/master/Coding%20Challenges/20180907-Weekly for details."
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

get '/game/v1/map/:timestamp/visualise(/:ids)?' do
    content_type 'text/html'
    hourcode = params['timestamp']
    if /^\d\d\d\d-\d\d-\d\d-\d\d$/.match(hourcode) then
        folderpath = "#{DATA_FOLDER_PATH}/#{hourcode}"
        if !File.exists?(folderpath) then
            status 404
            ""
        else
            mapfilepath = "#{folderpath}/map.json"
            map = JSON.parse(IO.read(mapfilepath))
            pointReprs = map['points'].map { |point|
              "<circle cx='#{(point['coordinates'][0] * 8) + 400}' cy='#{400 - (point['coordinates'][1] * 8)}' r='3' fill='black' />\n"
            }
            if params['ids'] then
              stops = params['ids'].split(",").each_cons(2)
              lineReprs = stops.map { |startLbl, finishLbl|
                start = GameLibrary::getPointForlabelAtMapOrNull(startLbl, map)
                finish = GameLibrary::getPointForlabelAtMapOrNull(finishLbl, map)
                if start && finish then
                  "<line x1='#{(start['coordinates'][0] * 8) + 400}' y1='#{400 - (start['coordinates'][1] * 8)}' x2='#{(finish['coordinates'][0] * 8) + 400}' y2='#{(400 - finish['coordinates'][1] * 8)}' style='stroke:rgb(255,0,0);stroke-width:2' />\n"
                else
                  ""
                end
              }
            else
              lineReprs = []
            end
            <<-eos
              <html>
              <head><title>#{map['mapId']}</title></head>
              <body>
                <p>#{map['timestamp']}</p>
                <svg style="margin: 10px; background-colour: #f7f7f7; border: solid 1px #ccc;" height="800" width="800">
                  <rect width="100%" height="100%" fill="#f7f7f7"/>
                  #{lineReprs.join}
                  #{pointReprs.join}
                </svg>
              </body>
              </html>
            eos
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

    if !GameLibrary::userSubmittedPathIsValidForMap(path, currentMap) then
        status 401
        return "Invalid path (are you using the correct labels)\n"
    end

    existingUserSubmissionOrNull = GameLibrary::existingUserSubmissionForThisHourOrNull(username)
    if existingUserSubmissionOrNull.nil? then
        data = GameLibrary::commitUserDataToDiskForThisHour(username, mapId, path)
        data["pathLength"] = GameLibrary::pathLengthAgainstMap(path, currentMap)
        JSON.pretty_generate(data)
    else
        existingUserSubmission = existingUserSubmissionOrNull
        existingPathAsString = existingUserSubmission["path"]
        proposedPathAsString = path
        existingPathLength = GameLibrary::pathLengthAgainstMap(existingPathAsString, currentMap)
        proposedPathLength = GameLibrary::pathLengthAgainstMap(proposedPathAsString, currentMap)
        if proposedPathLength < existingPathLength then
            data = GameLibrary::commitUserDataToDiskForThisHour(username, mapId, proposedPathAsString)
            data["pathLength"] = proposedPathLength
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
                        GameLibrary::pathLengthAgainstMap(u1["path"], map) <=> GameLibrary::pathLengthAgainstMap(u2["path"], map)
                    }
                score = 0.1/0.7
                lastlength = nil
                [
                    "",
                    File.basename(hoursFolderpath),
                    userSubmissionOrdered.map{|u|
                        currentUserLength = GameLibrary::pathLengthAgainstMap(u["path"], map).round(3)
                        if currentUserLength != lastlength then
                            score = score*0.7 
                        end
                        lastlength = currentUserLength
                        users = addScoreToUserLambda.call(users, u["username"], score)
                        "#{u["username"].ljust(20)} , length: #{currentUserLength} , score: #{score.round(3)}"
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
