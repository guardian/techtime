
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
    
end

