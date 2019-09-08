# git-sub

git-sub aims to provide a simple way to deal with submodules in a git repository
eliminating the need for manual operations 

the cli api tries to stay consistent with git and other cli tools by 
offering the following endpoints


```
git-sub add URL [PATH] - add a submodule
git-sub rm [PATH] - remove a submodule
git-sub mv [fromPATH] [toPATH] - move a submodule to a new directory
git-sub list - list submodules for the current repository

```

## roadmap

- [x] - implement list functionality
- [] - implement add functionality
- [] - implement remove functionality
- [] - implement move functionality
- [] - make search for .gitmodules and .git/config files possible from subdirectories




