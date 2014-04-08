include apt

# Update Apt before installing packages
Class['apt::update'] -> Package <| |>

# Drop apt::update from Package default, to avoid a cyclic dependency on
# apt::update, because apt::update in turn will depend on this package,
# because it has the tools to add PPAs
package { 'python-software-properties':
  ensure  => latest,
  require => [],
  before  => Class['apt'],
}


# Actual stuff we need..

apt::ppa { ['ppa:cassou/emacs', 'ppa:arankine/ninja-build']: }

package { 'cmake':
  ensure => latest
}

package { ['emacs24-common', 'emacs24-bin-common', 'emacs24-nox']:
  ensure  => latest,
  require => Apt::Ppa['ppa:cassou/emacs']
}

package { 'ninja-build':
  ensure => latest,
  require => Apt::Ppa['ppa:arankine/ninja-build']
}

$cask_version = '0.6.0'
$cask_archive = "cask-${cask_version}"

archive { $cask_archive:
  ensure        => present,
  digest_type   => 'sha1',
  digest_string => '6f836bb41f034d6be9611ee09c78eb8cc52b53ca',
  url           => "https://github.com/cask/cask/archive/v${cask_version}.tar.gz",
  target        => '/opt/',
}

file { '/usr/local/bin/cask':
  ensure  => link,
  target  => "/opt/${cask_archive}/bin/cask",
  require => Archive[$cask_archive],
}
