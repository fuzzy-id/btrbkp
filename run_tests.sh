#!/bin/sh
# All checks happen in the test main functions…

# This is how you can create an appropriate testing volume:
# $ dd if=/dev/zero of=/tmp/test_dev bs=1M count=100
# $ mkfs.btrfs /tmp/test_dev
# # mount -t btrfs -o user_subvol_rm_allowed /tmp/test_dev /mnt
# # btrfs sub cr /mnt/tests
# # chown USER.USER /mnt/tests
# $ run_tests.sh /mnt/tests

if [ ${#} != 1 ]; then
    echo "Usage: ${0} VOL"
    echo
    echo "Runs the tests passing VOL as directory"
    echo "to run btrfs tests in."
else
    cabal test --show-details=always --test-option=--test-vol --test-option="${1}"
fi
