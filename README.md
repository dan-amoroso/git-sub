# git-sub

git-sub tries to minimize manual operations needed to deal with submodules

the CLI api offers the following endpoints:


```
git-sub add URL PATH - add a submodule
git-sub rm PATH - remove a submodule
git-sub mv FROM-PATH TO-PATH - move a submodule to a new directory
git-sub list - list submodules for the current repository

```

## roadmap

- [x] - implement list functionality
- [x] - implement add functionality
- [x] - implement remove functionality
- [ ] - implement move functionality
- [ ] - make search for .gitmodules and .git/config files possible from subdirectories




