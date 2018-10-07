
# encoding: UTF-8

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
        UserFleet::commitFleetToDisk(currentHour, username, userFleet)
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

    # GameLibrary::makeErrorAnswer(httpstatus, errorcode, errormessage)
    def self.makeErrorAnswer(httpstatus, errorcode, errormessage)
        {
            "status"   => httpstatus,
            "error"    => errorcode,
            "message"  => errormessage
        }
    end

end
