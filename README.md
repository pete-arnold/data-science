# data-science

This is a set of example code files with some common (for practice) and uncommon
(for scope) R components for exploration and extension as well as practice with
pushing and pulling to a github repository.

Also included are previous sample R files and some of the module lecture notes
about software practices and functions.

## Recovering from changes

### Create a branch at the clean code.
git checkout ad1cc3a1 -b recover_from_changes

### Merge with the master branch, but keep the branch code.
git merge --strategy=ours master

### Merge the branch with the master.
git checkout master
git merge recover_from_changes

---
