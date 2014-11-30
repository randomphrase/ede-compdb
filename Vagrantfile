# -*- mode: ruby -*-
# vi: set ft=ruby :

# Vagrantfile API/syntax version. Don't touch unless you know what you're doing!
VAGRANTFILE_API_VERSION = "2"

Vagrant.configure(VAGRANTFILE_API_VERSION) do |config|
  config.vm.box = "hashicorp/precise64"

  config.vm.network "public_network", :bridge => 'en0: Ethernet 1'

  config.ssh.forward_agent = true

  # Bootstrap installs puppet 3 from the puppetlabs repo,and some modules
  config.vm.provision "ansible" do |ansible|
    ansible.playbook = "playbook.yml"
  end
end
