# package { 'ninja-build':
#   ensure => installed
# }

include apt

package { 'cmake':
  ensure => installed
}

apt::ppa { 'ppa:cassou/emacs': }

package { ['emacs24-common', 'emacs24-bin-common', 'emacs24-nox']:
  ensure  => latest,
  require => Apt::Ppa['ppa:cassou/emacs']
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
