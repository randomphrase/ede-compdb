# -*- mode: ruby -*-
# vi: set ft=ruby :

# Vagrantfile API/syntax version. Don't touch unless you know what you're doing!
VAGRANTFILE_API_VERSION = "2"

Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|
  config.vm.box = "hashicorp/precise64"

  config.vm.network "public_network", :bridge => 'en0: Ethernet 1'

  config.ssh.forward_agent = true

  # Bootstrap installs puppet 3 from the puppetlabs repo,and some modules
  config.vm.provision "puppet" do |puppet|
    puppet.manifests_path = 'puppet/manifests'
    puppet.manifest_file = "bootstrap.pp"
  end

  # Actually install the test environment
  config.vm.provision "puppet" do |puppet|
    puppet.manifests_path = 'puppet/manifests'
    puppet.manifest_file = "testenv.pp"
  end
end
