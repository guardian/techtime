
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

require_relative "MapUtils.rb"

# --  --------------------------------------------------

class Challenge20180927

    # Challenge20180927::ensureStructure()
    def self.ensureStructure()
        if !$STRUCTURE then
            if File.exists?(CHALLENGE_DATA_STRUCTURE_FILEPATH) then
                $STRUCTURE = JSON.parse(IO.read(CHALLENGE_DATA_STRUCTURE_FILEPATH))
            else
                $STRUCTURE = {}
            end
        end
        currentHour = GameLibrary::hourCode() 
        if $STRUCTURE[currentHour].nil? then
            $STRUCTURE[currentHour] = {}
            map = MapUtils::getCurrentMap()
            map["points"] = map["points"].first(100)
            $STRUCTURE[currentHour]["map"] = map
            $STRUCTURE[currentHour]["userSubmissions"] = {}
        end
    end

    # Challenge20180927::commitStructureToDisk()
    def self.commitStructureToDisk()
        return if $STRUCTURE.nil?
        File.open(CHALLENGE_DATA_STRUCTURE_FILEPATH, "w"){ |f| f.puts(JSON.pretty_generate($STRUCTURE)) }
        File.open(CHALLENGE_DATA_STRUCTURE_FILEPATH.gsub("structure.json", "structure-#{GameLibrary::hourCode()}.json"), "w"){ |f| f.puts(JSON.pretty_generate($STRUCTURE)) }
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
                "radius" => radius
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
            MapUtils::distanceBetweenTwoMapPoints(disk1["point"], disk2["point"]) >= disk1["radius"]+disk2["radius"]
        }
    end

    # Challenge20180927::collectionValue(disks)
    def self.collectionValue(disks)
        disks.map{|disk| disk["radius"]**2 }.inject(0, :+)
    end

end

# --  --------------------------------------------------

submission = IO.read("submission.txt")
$STRUCTURE = JSON.parse(IO.read("structure.json"))
currentHour = "2018-10-03-19"
map = $STRUCTURE[currentHour]["map"]
disks = Challenge20180927::makeDisksOrNull(map, submission)
if disks.nil? then
    raise "403: Your submission does not resolve for for this map \n"
end
if !Challenge20180927::hasLessThan50Disks(disks) then
    raise "403: You are submitting too many disks \n"
end
if !Challenge20180927::individualDisksAreValid(map, disks) then
    raise "403: At least one of your disks is not valid (intersects the map in non trivial ways) \n"
end
if !Challenge20180927::collectionIsValid(disks) then
    raise "403: Your collection is not valid (intersecting disks) \n"
end
puts "Looks fine"