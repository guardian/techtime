
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
            "shield-level" => 1,
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

    # UserFleet::topUpEnergyValue(currentHour, username, topUpValue)
    def self.topUpEnergyValue(currentHour, username, topUpValue)
        userFleet = UserFleet::getUserFleetDataOrNull(currentHour, username)
        currentLevel = userFleet["ship-inventory"]["capital"]["energy-level"]
        userFleet["ship-inventory"]["capital"]["energy-level"] = currentLevel + topUpValue
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

end

