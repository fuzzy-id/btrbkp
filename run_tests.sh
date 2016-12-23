#!/bin/sh
# All checks happen in the test main functions…

set -e

# This is how you can create an appropriate testing volume:
# $ dd if=/dev/zero of=/tmp/test_dev bs=1M count=100
# $ mkfs.btrfs /tmp/test_dev
# # mount -t btrfs -o user_subvol_rm_allowed /tmp/test_dev /mnt
# # btrfs sub cr /mnt/tests
# # chown USER.USER /mnt/tests
# $ run_tests.sh /mnt/tests

if [[ ${#} == 0 || ${#} > 2 ]]; then
    echo "Usage: ${0} [setup|VOL]"
    echo
    echo "If 'setup' is the only argument then a temporary file is"
    echo "created, formated as btrfs and mounted on a temporary"
    echo "directory."
    echo
    echo "Otherwise runs the tests passing VOL as directory"
    echo "to run btrfs tests in."
    exit 1
elif [[ "${1}" == "setup" ]]; then
    DEV="$(mktemp --tmpdir btrbkp-test-dev-XXX)"
    MNT="$(mktemp --tmpdir --directory btrbkp-test-mnt-XXX)"
    VOL="${MNT}/tests"
    dd if=/dev/zero of="${DEV}" bs=1M count=100
    mkfs.btrfs "${DEV}"
    sudo mount -t btrfs -o user_subvol_rm_allowed "${DEV}" "${MNT}"
    sudo btrfs subvolume create "${VOL}"
    sudo chown ${USER}.${USER} "${VOL}"
    echo "Run tests via"
    echo
    echo "${0} '${VOL}'"
else
    cabal test \
          --show-details=always \
          --test-option=--test-vol \
          --test-option="${1}"
fi
