
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

    # UserFleet::filepathToUserFleetData(currentHour, username)
    def self.filepathToUserFleetData(currentHour, username)
        filecode = Digest::SHA1.hexdigest(username)[0,8]
        "#{GAME_DATA_FOLDERPATH}/Timeline/#{currentHour}/fleets/#{filecode}.json"
    end

    # UserFleet::getUserFleetDataOrNull(currentHour, username)
    def self.getUserFleetDataOrNull(currentHour, username)
        fleetFilepath = UserFleet::filepathToUserFleetData(currentHour, username)
        return nil if !File.exists?(fleetFilepath)
        JSON.parse(IO.read(fleetFilepath))
    end

    # UserFleet::getUserFleetDataOrNull(currentHour, username) : Boolean # return false if UserFleet::getUserFleetDataOrNull(currentHour) return Null
    def self.trueIfUserFleetIsAlive(currentHour, username)
        fleetdata = UserFleet::getUserFleetDataOrNull(currentHour, username)
        return false if fleetdata.nil?
        fleetdata["ship-inventory"]["capital"]["alive"]
    end

    # UserFleet::spawnCapitalShipTopUpChallenge(difficulty)
    def self.spawnCapitalShipTopUpChallenge(difficulty)
        {
            "input"      => SecureRandom.hex,
            "difficulty" => difficulty
        }
    end

    # UserFleet::spawnCapitalShip(mapPoint, energyLevel, topUpChallengeDifficulty)
    def self.spawnCapitalShip(mapPoint, energyLevel, topUpChallengeDifficulty)
        {
            "nomenclature" => "capital-ship",
            "location"     => mapPoint,
            "energy-level" => energyLevel,
            "energy-top-up-challenge" => UserFleet::spawnCapitalShipTopUpChallenge(topUpChallengeDifficulty),
            "alive"        => true
        }
    end

    # UserFleet::spawnUserFleet(username, mapPoint, capitalShipInitialEnergy, topUpChallengeDifficulty)
    def self.spawnUserFleet(username, mapPoint, capitalShipInitialEnergy, topUpChallengeDifficulty)
        capitalShip = UserFleet::spawnCapitalShip(mapPoint, capitalShipInitialEnergy, topUpChallengeDifficulty)
        {
            "username" => username,
            "in-play" => true,
            "score" => 0,
            "ship-inventory" => {
                "capital" => capitalShip,
                "battle-cruisers" => [],
                "energy-carriers" => []
            },
            "log-warnings" => []
        }
    end

    # UserFleet::validateTopUpCode(currentHour, username, code)
    def self.validateTopUpCode(currentHour, username, code)
        userFleet = UserFleet::getUserFleetDataOrNull(currentHour, username)
        capital = userFleet["ship-inventory"]["capital"]
        challenge = capital["energy-top-up-challenge"]
        # {
        #    "input"      => String
        #    "difficulty" => Integer
        # }
        Digest::SHA1.hexdigest("#{challenge["input"]}#{code}")[-challenge["difficulty"], challenge["difficulty"]] == ("0"*challenge["difficulty"])
    end

    # UserFleet::topUpCapitalShipAndResetTopUpChallenge(currentHour, username, topUpValue, difficulty)
    def self.topUpCapitalShipAndResetTopUpChallenge(currentHour, username, topUpValue, difficulty)
        userFleet = UserFleet::getUserFleetDataOrNull(currentHour, username)
        currentLevel = userFleet["ship-inventory"]["capital"]["energy-level"]
        userFleet["ship-inventory"]["capital"]["energy-level"] = currentLevel + topUpValue
        userFleet["ship-inventory"]["capital"]["energy-top-up-challenge"] = UserFleet::spawnCapitalShipTopUpChallenge(difficulty)
        UserFleet::commitFleetToDisk(currentHour, username, userFleet)
    end

    # UserFleet::commitFleetToDisk(currentHour, username, fleet)
    def self.commitFleetToDisk(currentHour, username, fleet)
        userFleetFilepath = UserFleet::filepathToUserFleetData(currentHour, username)
        if !File.exists?(File.dirname(userFleetFilepath)) then
            FileUtils.mkpath File.dirname(userFleetFilepath) # we do this because the fleet, subfolder of a timeline hours folder is not automatically created
        end 
        File.open(userFleetFilepath, "w"){|f| f.puts(JSON.pretty_generate(fleet)) }
    end

    # UserFleet::spawnBattleCruiser(mapPoint, initialEnergyLevel)
    def self.spawnBattleCruiser(mapPoint, initialEnergyLevel)
        {
            "nomenclature" => "battle-cruiser",
            "ship-uuid"    => SecureRandom.uuid,
            "location"     => mapPoint,
            "energy-level" => initialEnergyLevel,
            "alive"        => true,
            "space-probe-results" => []
        }
    end

    # UserFleet::spawnEnergyCarrier(mapPoint, initialEnergyLevel)
    def self.spawnEnergyCarrier(mapPoint, initialEnergyLevel)
        {
            "nomenclature" => "energy-carrier",
            "ship-uuid"    => SecureRandom.uuid,
            "location"     => mapPoint,
            "energy-level" => initialEnergyLevel,
            "alive"        => true
        }
    end

    # UserFleet::insertOrUpdateShipAtFleet(fleet, ship)
    def self.insertOrUpdateShipAtFleet(fleet, ship)
        if ship["nomenclature"] == "battle-cruiser" then
            fleet["ship-inventory"]["battle-cruisers"] = fleet["ship-inventory"]["battle-cruisers"].reject{|s| s["ship-uuid"]==ship["ship-uuid"] }
            fleet["ship-inventory"]["battle-cruisers"] << ship
        end
        if ship["nomenclature"] == "energy-carrier" then
            fleet["ship-inventory"]["energy-carriers"] = fleet["ship-inventory"]["energy-carriers"].reject{|s| s["ship-uuid"]==ship["ship-uuid"] }
            fleet["ship-inventory"]["energy-carriers"] << ship
        end
        fleet
    end

end

