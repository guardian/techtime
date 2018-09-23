
# encoding: UTF-8

class UserKeys

    # UserKeys::getUserKeysData()
    def self.getUserKeysData()
        userkeysFilepath = "#{GAME_DATA_FOLDERPATH}/users-keys.txt"
        userKeysData = IO.read(userkeysFilepath)
                            .lines.map{|line| line.strip }
                            .select{|line| line.size>0 }
                            .map{|line| line.split(":") }
        userKeysData
    end

    # UserKeys::commitUserKey(username, userkey)
    def self.commitUserKey(username, userkey)
        userkeysFilepath = "#{GAME_DATA_FOLDERPATH}/users-keys.txt"
        File.open(userkeysFilepath, "a"){|f| f.puts("#{username}:#{userkey}") }
    end

    # UserKeys::validateUserCredentials(username, userkey) 
    def self.validateUserCredentials(username, userkey)
        UserKeys::getUserKeysData().any?{|record| record[0]==username and record[1]==userkey }
    end

    # UserKeys::getUsernameFromUserkeyOrNull(userkey) 
    def self.getUsernameFromUserkeyOrNull(userkey)
        UserKeys::getUserKeysData().each{|pair|
            return pair[0] if pair[1]==userkey
        }
        nil
    end

end
