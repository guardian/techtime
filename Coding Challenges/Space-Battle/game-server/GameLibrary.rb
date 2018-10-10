
# encoding: UTF-8

require 'find'
require 'fileutils'
# FileUtils.mkpath '/a/b/c'
# FileUtils.cp(src, dst)
# FileUtils.mv 'oldname', 'newname'
# FileUtils.rm(path_to_image)
# FileUtils.rm_rf('dir/to/remove')

# ----------------------------------------------------------

class GameLibrary

    # GameLibrary::hourCode()
    def self.hourCode()
        Time.new.strftime("%Y-%m-%d-%H")
    end

    # GameLibrary::makeGameAtHourDataFolderPathForGivenHourcode(hourCode)
    def self.makeGameAtHourDataFolderPathForGivenHourcode(hourCode)
        # hourCode exmaple 2018-10-07-11
        folderpath = "#{GAME_DATA_FOLDERPATH}/Timeline/#{hourCode[0,4]}/#{hourCode[0,7]}/#{hourCode}"
        if !File.exists?(folderpath) then
            FileUtils.mkpath folderpath
        end
        folderpath
    end

    # GameLibrary::getGameAtHoursDataFolderPaths()
    def self.getGameAtHoursDataFolderPaths()
        folderpaths = []
        Find.find("#{GAME_DATA_FOLDERPATH}/Timeline") do |path|
          next if File.file?(path)
          next if File.basename(path).start_with?(".")
          next if File.basename(path).size != "2018-10-07-11".size
          folderpaths << path
        end
        folderpaths
    end

    # GameLibrary::getMapAtHourFolderpath(folderpath)
    def self.getMapAtHourFolderpath(folderpath)
        mapfilepath = "#{folderpath}/map.json"
        JSON.parse(IO.read(mapfilepath))
    end

    # GameLibrary::ensureGameFolderSetUpForThisHour()
    def self.ensureGameFolderSetUpForThisHour()

        hourCode = GameLibrary::hourCode()
        folderpath = GameLibrary::makeGameAtHourDataFolderPathForGivenHourcode(hourCode)

        mapfilepath = "#{folderpath}/map.json"
        return folderpath if File.exists?(mapfilepath)

        # ---------------------------------------
        # The Map
        map = {}
        map["mapId"] = SecureRandom.uuid
        map["timestamp"] = hourCode
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
        UserFleet::commitFleetToDisk(hourCode, username, userFleet)

        # ---------------------------------------

        folderpath

    end

    # GameLibrary::doUserFleetPointIncreaseForShipDestroyed(hourCode, username, nomenclature)
    def self.doUserFleetPointIncreaseForShipDestroyed(hourCode, username, nomenclature)
        userFleet = UserFleet::getUserFleetDataOrNull(hourCode, username)
        userFleet = ScoringUtils::userFleetPointIncreaseForShipDestroyed(userFleet, nomenclature)
        UserFleet::commitFleetToDisk(hourCode, username, userFleet)
    end

    # GameLibrary::userFleetsForHour(hourCode)
    def self.userFleetsForHour(hourCode)
        gameAtHourDataFolderPath = GameLibrary::makeGameAtHourDataFolderPathForGivenHourcode(hourCode)
        Dir.entries("#{gameAtHourDataFolderPath}/fleets")
            .select{|filename| filename[-5,5]==".json" }
            .map{|filename| "#{gameAtHourDataFolderPath}/fleets/#{filename}" }
            .map{|filepath| JSON.parse(IO.read(filepath)) }
    end

    # GameLibrary::make200Answer(answer, hourCode, username)
    def self.make200Answer(answer, hourCode, username)
        {
            "status" => 200,
            "answer" => answer,
            "userFleet" => UserFleet::getUserFleetDataOrNull(hourCode, username)
        }
    end

    # GameLibrary::makeErrorAnswer(httpstatus, errorcode, errormessage)
    def self.makeErrorAnswer(httpstatus, errorcode, errormessage)
        {
            "status"   => httpstatus,
            "error"    => errorcode,
            "message"  => errormessage
        }
    end

end
