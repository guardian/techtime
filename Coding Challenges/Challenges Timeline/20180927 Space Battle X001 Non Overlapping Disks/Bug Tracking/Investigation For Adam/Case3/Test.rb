
# encoding: UTF-8

# ------------------------------------------------------------
# utils

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
            if mapPoint.nil? then
                puts "label: #{label}"
                puts "radius: #{radius}"
            end
            return nil if mapPoint.nil?
            disk = {
                "point"  => mapPoint,
                "radius" => radius.round(12)
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

# --  --------------------------------------------------

submission = IO.read("submission.txt")
$STRUCTURE = JSON.parse(IO.read("structure.json"))
currentHour = "2018-10-09-10"
map = $STRUCTURE["map"]
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