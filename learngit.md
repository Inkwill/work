Git is a distributed version control system.
Git is a free software distributed under the GPL.
Git has a mutable index called stage. 
Git tracks changes of files.  
Creating a new branch is quick AND simple.  

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

### delete file  
`git rm filename`  

### create SSH-Key 
`ssh-keygen -t rsa -C "youremail@example.com"`  

### add remote stage  
`git remote add origin git@github.com:inkwill/hello-github.git`  

### push local stage to empty remote  
`git push -u origin master`  

### clone remote stage to local  
`git clone git@github.com:inkwill/hello-github.git`  

### check current branch  
`git branch`  

### creat and checkout branch  
`git branch <branch-name>`  
`git checkout <branch-name>`  
`git switch <branch-name>`  (**recommend**)  
`git checkout -b <branch-name>` (== branch + checkout)  
`git switch -c <branch-name>` (**recommend**)

### merge branch  
`git merge <branch-name>`  

### delete branch  
`git branch -d <branch-name>`  

