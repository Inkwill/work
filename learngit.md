Git is a distributed version control system.
Git is a free software distributed under the GPL.
Git has a mutable index called stage. 
Git tracks changes of files.  

### creat Git repository 
`git init`  

### add file/modify  
`git add filename`  

### commit file  
`git commit -m "description of commit" `  

### check status  
`git status`  

### check modify  
`git diff (filename)`  

### check log  
`git log (--pretty=oneline)`  

### revert to a version  
`git reset --hard versionID(HEAD^、HEAD~100、commit id)`  

### check histrory log  
`git reflog`  

### revert modify from working directory (== checkout the HEAD version)  
`git checkout -- filename`  

### unstage modify from stage directory  
`git reset HEAD filename`  

