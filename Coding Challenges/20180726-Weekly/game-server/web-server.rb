
# encoding: UTF-8

# ------------------------------------------------------------
# utils

require 'sinatra'
# http://www.sinatrarb.com/intro.html

require 'securerandom'
# SecureRandom.hex    #=> "eb693ec8252cd630102fd0d0fb7c3485"
# SecureRandom.hex(2) #=> "eb69"
# SecureRandom.uuid   #=> "2d931510-d99f-494a-8c67-87feb05e1594"

require 'json'

require 'find'

# --  --------------------------------------------------

set :port, 14001
#set :public_folder, "path/to/www"

# -- --------------------------------------------------

DATA_FOLDER_PATH = "/Galaxy/DataBank/IPD-Challenge-Data"
LUCILLE_INSTANCE = ENV["COMPUTERLUCILLENAME"]

if LUCILLE_INSTANCE.nil? then
    puts "Error: Environment variable 'COMPUTERLUCILLENAME' is not defined."
    exit
end

class Utils

    # Utils::getNonEmptyLinesStrippedFromFile(filepath)
    def self.getNonEmptyLinesStrippedFromFile(filepath)
        IO.read(filepath)
            .lines
            .map{|line| line.strip }
            .select{|line| line.size>0 }       
    end

    # Utils::spawnGame(gameId, partyName, counterPartyName)
    def self.spawnGame(gameId, partyName, counterPartyName)
        {
            partyName  => [],
            counterPartyName =>  [],
            "scores" => nil,
            "game_metadata" => {
                "game_id"       => gameId,
                "starting_date" => Time.new.to_s,
                "players"       => [partyName, counterPartyName],
                "status"        => "on-going"
            }
        }        
    end
end

class IPD # Pure static functions only

    # IPD::playerNameToAverageGameScoreOrNull(playername)
    def self.playerNameToAverageGameScoreOrNull(playername)
        # TODO: Implement this
        0
    end

    # IPD::getNameOfTheOtherPlayer(name, game)
    def self.getNameOfTheOtherPlayer(name, game)
        (game["game_metadata"]["players"] - [name]).first
    end

    # IPD::reduceGameMovesVisibility(game, playerName)
    def self.reduceGameMovesVisibility(game, playerName)
        playerMoves = game[playerName]
        otherPlayerName = IPD::getNameOfTheOtherPlayer(playerName, game)
        otherPlayerMoves = game[otherPlayerName]
        game[otherPlayerName] = otherPlayerMoves.first(playerMoves.size)
        game
    end

    # IPD::trueGameAsCompleted(game)
    def self.trueGameAsCompleted(game)
        game["game_metadata"]["players"]
            .all?{|name| game[name].size==10 }
    end

    # IPD::markCompletionAndScoresIfNeeded(game)
    def self.markCompletionAndScoresIfNeeded(game)
        if IPD::trueGameAsCompleted(game) then
            game["game_metadata"]["status"] = "completed"
            names = game["game_metadata"]["players"]
            name1 = names[0]
            name2 = names[1]
            scores = {}
            scores[name1] = 0
            scores[name2] = 0
            (0..9).each{|i|
                move1 = game[name1][i]
                move2 = game[name2][i]
                if move1==1 and move2==1 then
                    scores[name1] = scores[name1] + 3
                    scores[name2] = scores[name2] + 3
                end
                if move1==1 and move2==0 then
                    scores[name1] = scores[name1] + 0
                    scores[name2] = scores[name2] + 5
                end
                if move1==0 and move2==1 then
                    scores[name1] = scores[name1] + 5
                    scores[name2] = scores[name2] + 0
                end
                if move1==0 and move2==0 then
                    scores[name1] = scores[name1] + 1
                    scores[name2] = scores[name2] + 1
                end
            }
            game["scores"] = scores
        end
        game
    end

    # IPD::gamePostDiskExtractionProcessing(game, playerName)
    def self.gamePostDiskExtractionProcessing(game, playerName)
        game = IPD::reduceGameMovesVisibility(game, playerName)
        game = IPD::markCompletionAndScoresIfNeeded(game)
        game
    end

end

class GameIO

    # GameIO::getPlayerIds(): Array[PlayerId] , PlayerId: example: "9ee49d8e-39f2:Alice.Skywalker"
    def self.getPlayerIds() 
        Utils::getNonEmptyLinesStrippedFromFile("#{DATA_FOLDER_PATH}/players.txt")
    end
    # GameIO::getPlayerNames(): Array[String]
    def self.getPlayerNames()
        GameIO::getPlayerIds().map{|playerId| playerId.split(":")[1] }
    end

    # GameIO::personalKeyToPlayerNameOrNull(personalkey): Option[String]
    def self.personalKeyToPlayerNameOrNull(personalkey)
        pairs = GameIO::getPlayerIds()
            .map{|playerId| playerId.split(":") }
            .select{|pair| pair[0]==personalkey }
        return nil if pairs.size==0
        pairs.first[1]
    end

    # GameIO::personalKeyIsCurrent(personalkey): Boolean
    def self.personalKeyIsCurrent(personalkey)
        !GameIO::personalKeyToPlayerNameOrNull(personalkey).nil?
    end

    # GameIO::putGameToDisk(game)
    def self.putGameToDisk(game)
        gameId = game["game_metadata"]["game_id"]
        filepath = "/Galaxy/DataBank/IPD-Challenge-Data/games/#{gameId}.game"
        File.open(filepath, "w"){|f| f.puts(JSON.pretty_generate(game)) }
    end

    # GameIO::getGameFromDiskOrNull(gameId)
    def self.getGameFromDiskOrNull(gameId)
        filepath = "/Galaxy/DataBank/IPD-Challenge-Data/games/#{gameId}.game"
        return nil if !File.exists?(filepath)
        JSON.parse(IO.read(filepath))
    end

    # GameIO::getGamesFromDisk()
    def self.getGamesFromDisk()
        games = []
        Find.find("/Galaxy/DataBank/IPD-Challenge-Data/games/") do |path|
            next if path[-5,5] != ".game"
            begin
                games << JSON.parse(IO.read(path))
            rescue
            end
        end
        games  
    end

    # GameIO::getProcessedUserGamesFromDisk(playerName)
    def self.getProcessedUserGamesFromDisk(playerName)
        GameIO::getGamesFromDisk()
            .select{|game|
                game["game_metadata"]["players"].include?(playerName)
            }
            .map{|game| IPD::gamePostDiskExtractionProcessing(game, playerName) } 
    end

end

# -- --------------------------------------------------
# Route

not_found do
  '404'
end

get '/' do
    "Iterated Prisoner's Dilemma (20180726-Weekly), see https://github.com/guardian/techtime/tree/master/Coding%20Challenges/20180726-Weekly for details.\n"
end

get '/server/ping' do
    content_type 'application/json'
    '["pong"]'
end

get '/game-board' do
    playernames = GameIO::getPlayerNames()
    board = {}
    GameIO::getPlayerNames().each{|playername|
        board[playername] = IPD::playerNameToAverageGameScoreOrNull(playername)
    }
    content_type 'application/json'
    JSON.generate(board)
end

get '/game/:personalkey/players' do
    personalkey = params['personalkey']
    if !GameIO::personalKeyIsCurrent(personalkey) then
        status 401
        return
    end
    content_type 'application/json'
    JSON.generate(GameIO::getPlayerNames())
end

get '/game/:personalkey/start/:playername' do
    personalkey  = params['personalkey']
    counterPartyName = params['playername']
    if !GameIO::personalKeyIsCurrent(personalkey) then
        status 401
        return
    end
    partyName = GameIO::personalKeyToPlayerNameOrNull(personalkey)
    if partyName.nil? then
        status 404
        return
    end
    if !GameIO::getPlayerNames().include?(counterPartyName) then
        status 404
        return
    end
    gameId = SecureRandom.uuid
    game = Utils::spawnGame(gameId, partyName, counterPartyName)
    GameIO::putGameToDisk(game)
    answer = {
        "event" => "Starting a game between you (#{partyName}) and #{counterPartyName}",
        "party" => partyName,
        "cuonterparty" => counterPartyName,
        "gameId" => gameId
    }
    content_type 'application/json'
    JSON.generate(answer)
end

get '/game/:personalkey/play/:gameid/cooperate' do
    personalkey  = params['personalkey']
    gameId = params['gameid']
    if !GameIO::personalKeyIsCurrent(personalkey) then
        status 401
        return
    end
    partyName = GameIO::personalKeyToPlayerNameOrNull(personalkey)
    if partyName.nil? then
        status 404
        return
    end
    game = GameIO::getGameFromDiskOrNull(gameId)
    if game.nil? then
        status 404
        return
    end  
    if !game["game_metadata"]["players"].include?(partyName) then
        status 401
        return
    end 
    content_type 'application/json'  
    if game[partyName].size < 10 then
        game[partyName] << 1
        GameIO::putGameToDisk(game)
        "[true]"
    else
        "[false]"
    end
end

get '/game/:personalkey/play/:gameid/betray' do
    personalkey  = params['personalkey']
    gameId = params['gameid']
    if !GameIO::personalKeyIsCurrent(personalkey) then
        status 401
        return
    end
    partyName = GameIO::personalKeyToPlayerNameOrNull(personalkey)
    if partyName.nil? then
        status 404
        return
    end
    game = GameIO::getGameFromDiskOrNull(gameId)
    if game.nil? then
        status 404
        return
    end
    if !game["game_metadata"]["players"].include?(partyName) then
        status 401
        return
    end 
    content_type 'application/json'
    if game[partyName].size < 10 then
        game[partyName] << 0
        GameIO::putGameToDisk(game)
        "[true]"
    else
        "[false]"
    end
end

get '/game/:personalkey/game-status/:gameid' do
    personalkey  = params['personalkey']
    gameId = params['gameid']
    if !GameIO::personalKeyIsCurrent(personalkey) then
        status 401
        return
    end
    partyName = GameIO::personalKeyToPlayerNameOrNull(personalkey)
    if partyName.nil? then
        status 404
        return
    end
    game = GameIO::getGameFromDiskOrNull(gameId)
    if game.nil? then
        status 404
        return
    end
    if !game["game_metadata"]["players"].include?(partyName) then
        status 401
        return
    end 
    content_type 'application/json'
    JSON.generate(IPD::gamePostDiskExtractionProcessing(game, partyName))
end

get '/game/:personalkey/my-games' do
    personalkey  = params['personalkey']
    if !GameIO::personalKeyIsCurrent(personalkey) then
        status 401
        return
    end
    partyName = GameIO::personalKeyToPlayerNameOrNull(personalkey)
    if partyName.nil? then
        status 404
        return
    end   
    content_type 'application/json'
    JSON.generate(GameIO::getProcessedUserGamesFromDisk(partyName))
end
