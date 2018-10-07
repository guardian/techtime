
# encoding: UTF-8

class DeploymentOperator
    # DeploymentOperator::folderHash(folderpath)
    def self.folderHash(folderpath)
        Dir.entries(folderpath)
            .select{|filename| filename[0,1] != "." }
            .map{|filename| "#{folderpath}/#{filename}" }
            .select{|filepath| File.file?(filepath) }
            .map{|filepath| "#{filepath}:#{Digest::SHA1.file(filepath).hexdigest}" }
            .join("::")
    end
    # DeploymentOperator::codeHash()
    def self.codeHash()
        hash1 = DeploymentOperator::folderHash(SERVER_FOLDERPATH)
        hash2 = DeploymentOperator::folderHash("#{SERVER_FOLDERPATH}/library")
        Digest::SHA1.hexdigest([hash1, hash2].join())
    end
end

$INITIAL_CODE_HASH = DeploymentOperator::codeHash()

=begin
    The logic here is that the server is keep alive by MacOSX's Launchd, so it is safe to 
    have it kill itself when the code is updated. Knowing it will (almost instantly) come back to life
    on the updated code. Hence "deployment".
=end

Thread.new {
    loop {
        sleep 60
        if $INITIAL_CODE_HASH != DeploymentOperator::codeHash() then
            Kernel.exit
        end 
    }
}
