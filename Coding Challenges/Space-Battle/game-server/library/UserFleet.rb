
# encoding: UTF-8

require 'securerandom'
# SecureRandom.hex    #=> "eb693ec8252cd630102fd0d0fb7c3485"
# SecureRandom.hex(2) #=> "eb69"
# SecureRandom.uuid   #=> "2d931510-d99f-494a-8c67-87feb05e1594"

require 'json'

require 'digest/sha1'
# Digest::SHA1.hexdigest 'foo'
# Digest::SHA1.file(myFile).hexdigest

class UserFleet

    # UserFleet::filepathToUserFleetData(hourCode, username)
    def self.filepathToUserFleetData(hourCode, username)
        gameAtHourDataFolderPath = GameLibrary::makeGameAtHourDataFolderPathForGivenHourcode(hourCode)
        filecode = Digest::SHA1.hexdigest(username)[0,8]
        "#{gameAtHourDataFolderPath}/fleets/#{filecode}.json"
    end

    # UserFleet::getUserFleetDataOrNull(hourCode, username)
    def self.getUserFleetDataOrNull(hourCode, username)
        fleetFilepath = UserFleet::filepathToUserFleetData(hourCode, username)
        return nil if !File.exists?(fleetFilepath)
        $usersFleetsIOActionsMutex.synchronize {
            JSON.parse(IO.read(fleetFilepath))
        }
    end

    # UserFleet::getUserFleetDataOrNull(hourCode, username) : Boolean # return false if UserFleet::getUserFleetDataOrNull(hourCode) return Null
    def self.trueIfUserFleetIsAlive(hourCode, username)
        fleetdata = UserFleet::getUserFleetDataOrNull(hourCode, username)
        return false if fleetdata.nil?
        fleetdata["ships"][0]["alive"]
    end

    # UserFleet::spawnCapitalTopUpChallenge(difficulty)
    def self.spawnCapitalTopUpChallenge(difficulty)
        {
            "input"      => SecureRandom.hex,
            "difficulty" => difficulty
        }
    end

    # UserFleet::spawnCapitalShip(mapPoint, energyLevel)
    def self.spawnCapitalShip(mapPoint, energyLevel)
        {
            "nomenclature" => "capitalShip",
            "uuid"     => SecureRandom.uuid,
            "location"     => mapPoint,
            "energyLevel"  => energyLevel,
            "alive"        => true
        }
    end

    # UserFleet::spawnUserFleet(username, mapPoint, capitalShipInitialEnergy, topUpChallengeDifficulty)
    def self.spawnUserFleet(username, mapPoint, capitalShipInitialEnergy, topUpChallengeDifficulty)
        capitalShip = UserFleet::spawnCapitalShip(mapPoint, capitalShipInitialEnergy)
        {
            "username"    => username,
            "capitalEnergyTopUpChallenge" => UserFleet::spawnCapitalTopUpChallenge(topUpChallengeDifficulty),
            "gameScore"   => 0,
            "ships"       => [ capitalShip ],
            "spaceProbeResults" => {},
            "logWarnings" => [],
            "mapExploration" => []
        }
    end

    # UserFleet::validateTopUpCode(hourCode, username, code)
    def self.validateTopUpCode(hourCode, username, code)
        userFleet = UserFleet::getUserFleetDataOrNull(hourCode, username)
        challenge = userFleet["capitalEnergyTopUpChallenge"]
        # {
        #    "input"      => String
        #    "difficulty" => Integer
        # }
        Digest::SHA1.hexdigest("#{challenge["input"]}#{code}")[-challenge["difficulty"], challenge["difficulty"]] == ("0"*challenge["difficulty"])
    end

    # UserFleet::topUpCapitalShipAndResetTopUpChallenge(hourCode, username, topUpValue, difficulty)
    def self.topUpCapitalShipAndResetTopUpChallenge(hourCode, username, topUpValue, difficulty)
        userFleet = UserFleet::getUserFleetDataOrNull(hourCode, username)
        currentLevel = userFleet["ships"][0]["energyLevel"]
        userFleet["ships"][0]["energyLevel"] = currentLevel + topUpValue
        userFleet["capitalEnergyTopUpChallenge"] = UserFleet::spawnCapitalTopUpChallenge(difficulty)
        UserFleet::commitFleetToDisk(hourCode, username, userFleet)
    end

    # UserFleet::commitFleetToDisk(hourCode, username, fleet)
    def self.commitFleetToDisk(hourCode, username, fleet)
        userFleetFilepath = UserFleet::filepathToUserFleetData(hourCode, username)
        if !File.exists?(File.dirname(userFleetFilepath)) then
            FileUtils.mkpath File.dirname(userFleetFilepath) # we do this because the fleet, subfolder of a timeline hours folder is not automatically created
        end 
        $usersFleetsIOActionsMutex.synchronize {
            File.open(userFleetFilepath, "w"){|f| f.puts(JSON.pretty_generate(fleet)) }
        }
    end

    # UserFleet::spawnBattleCruiser(mapPoint, initialEnergyLevel)
    def self.spawnBattleCruiser(mapPoint, initialEnergyLevel)
        {
            "nomenclature" => "battleCruiser",
            "uuid"     => SecureRandom.uuid,
            "location"     => mapPoint,
            "energyLevel"  => initialEnergyLevel,
            "alive"        => true,
        }
    end

    # UserFleet::spawnEnergyCarrier(mapPoint, initialEnergyLevel)
    def self.spawnEnergyCarrier(mapPoint, initialEnergyLevel)
        {
            "nomenclature" => "energyCarrier",
            "uuid"     => SecureRandom.uuid,
            "location"     => mapPoint,
            "energyLevel"  => initialEnergyLevel,
            "alive"        => true
        }
    end

    # UserFleet::insertOrUpdateShipAtFleet(fleet, ship)
    def self.insertOrUpdateShipAtFleet(fleet, ship)
        fleet["ships"] = fleet["ships"].reject{|s| s["uuid"]==ship["uuid"] }
        fleet["ships"] << ship
        # Now, we need to make sure that capital comes first
        capitalSingleton, otherShips = fleet["ships"].partition{|s| s["nomenclature"] == "capitalShip" }
        fleet["ships"] = capitalSingleton + otherShips
        fleet
    end

    # UserFleet::distanceBetweenTwoMapPoints(point1, point2)
    def self.distanceBetweenTwoMapPoints(point1, point2)
        dx = point1["coordinates"][0] - point2["coordinates"][0]
        dy = point1["coordinates"][1] - point2["coordinates"][1]
        Math.sqrt( (dx**2) + (dy**2) )
    end

    # UserFleet::userShipsWithinDisk(hourCode, username, mapPoint, radius) # radius in kilometers
    def self.userShipsWithinDisk(hourCode, username, mapPoint, radius)
        userFleet = UserFleet::getUserFleetDataOrNull(hourCode, username)
        userFleet["ships"].select{|ship|
            UserFleet::distanceBetweenTwoMapPoints(ship["location"], mapPoint) <= radius
        }
    end 

    # UserFleet::spawnWormholeBombWarningLogItem(attackerMapPoint, attackerUsername, targetShip)
    def self.spawnWormholeBombWarningLogItem(attackerMapPoint, attackerUsername, targetShip)
        {
            "unixtime"  => Time.new.to_f,
            "eventUUID" => SecureRandom.uuid,
            "eventType" => "WormholeBomb",
            "eventData" => {
                "source" => {
                    "location"     => attackerMapPoint,
                    "nomenclature" => "BattleCruiser",
                    "username"     => attackerUsername
                },
                "target" => targetShip
            }
        }        
    end

    # UserFleet::registerShipTakingBombImpact(userFleet, attackerMapPoint, attackerUsername, targetShip, bombEffectiveEnergy)
    def self.registerShipTakingBombImpact(userFleet, attackerMapPoint, attackerUsername, targetShip, bombEffectiveEnergy)
        attackerBombDamageReportItem = nil
        return [userFleet, targetShip, attackerBombDamageReportItem] if !targetShip["alive"] 
        if targetShip["nomenclature"] == "energyCarrier" then
            targetShip["energyLevel"] = 0
            targetShip["alive"] = false
        else
            targetShip["energyLevel"] = targetShip["energyLevel"] - bombEffectiveEnergy
            if targetShip["energyLevel"] < 0 then
                targetShip["alive"] = false
            end
        end
        attackerBombDamageReportItem = {
            "username"     => userFleet["username"],
            "nomenclature" => targetShip["nomenclature"],
            "alive"        => targetShip["alive"]
        }
        userFleet["logWarnings"] << UserFleet::spawnWormholeBombWarningLogItem(attackerMapPoint, attackerUsername, targetShip)
        [userFleet, targetShip, attackerBombDamageReportItem]
    end

    def self.getShipPerUUIDOrNull(hourCode, username, shipuuid)
        userFleet = UserFleet::getUserFleetDataOrNull(hourCode, username)
        return nil if userFleet.nil?
        userFleet["ships"].select{|ship| ship["uuid"]==shipuuid }.first
    end

end

