---
title: Static Comments with Hakyll
date: 2017-10-07
tags: hakyll, haskell, staticman, commenting
author: Abhinav Sarkar
toc: right
---

Recently, I set up this blog[^source] to run on [Hakyll] which is a static site generator library written in [Haskell]. It is very configurable and uses a DSL[^DSL] to write the site configuration.

Even though the blog is generated statically with Hakyll, if you want the users to comment on your posts, you generally need to integrate a third-party commenting system like [Disqus] or [Facebook]. With these third-party system, you don't have the comment data in your control anymore. There are some open source alternatives to them, most notably [Isso], which you have to host on a server on your own. But even then, the comments are not actually "static"; they live in a database on your hosted server.

Fortunately, there is a solution which does exactly what we want: [Staticman]. In this post I'll go over how to integrate a Hakyll blog hosted on [Github pages][1] with Staticman to get truly static commenting.

<!--more-->

* toc

## My Hakyll Blog

We start with creating a simple blog. First make sure that you have Hakyll and [_stack_] installed. Then run this command on the command-line to create a new blog:

```bash
$ hakyll-init hakyll-staticman
```

You now have a shiny new Hakyll powered blog. Go ahead and try it out:

```bash
$ cd hakyll-staticman
$ stack init
$ stack build
$ stack exec site build
$ stack exec site server
```

You should be able to see the blog at <http://127.0.0.1:8000>. This is a sample blog which comes bundled with Hakyll but it will suffice for our demonstration purpose.

![My Hakyll Blog](/images/static-comments-with-hakyll/simple-blog.png)

## Github Pages Setup

Since the blog needs to be hosted on Github Pages, let's first set that up. Go to Github and create a new repository. In this post, for demonstration, I'll use a repository of [my own][2]. Make sure to check "Initialize this repository with a README" so that the _master_ branch is created automatically.

![Creating the Github Repo](/images/static-comments-with-hakyll/create-repo.png)

Next, go to your blog's Github repo and click the "Settings" tab. Scroll down to the "Github Pages" section, select "master branch" from the "Source" dropdown and click on the "Save" button.

![Enabling Github Pages](/images/static-comments-with-hakyll/enable-github-pages.png)

Next, git initialize your blog and create a new orphan branch called _source_. This branch will have the source for the blog, the post and the comments, while the _master_ branch will have the generated site.

```bash
$ rm -rf _site
$ git init
$ git remote add origin git@github.com:abhin4v/hakyll-staticman.git
$ git checkout --orphan source
$ git submodule add --force https://github.com/abhin4v/hakyll-staticman.git _site
$ echo "_site" >> .git/info/exclude
$ cat << EOF > .gitignore
_cache
.stack-work
EOF
$ git add .
$ git commit -m "Setup blog"
$ git push -u origin source
```

We make `_site` directory (where Hakyll puts the generated site) a git submodule pointing to the same repo's _master_ branch. After adding a `.gitignore` file, we commit all the files and push it to the _source_ branch of the repo. We also add `_site` to the `.git/info/exclude` file to locally ignore the changes to it.

## Continuous Commenting
The plan is to use a CI/CD[^CICD] tool to automatically build the website on push to the _source_ branch and deploy it to the _master_ branch. This is so that you don't have to deploy the site manually when comments are added. I use [Travis CI][3] for my blog so I'm going to show you how to configure the same. If you wish to use a different tool, please consult its documentation for the equivalent configuration.

Add the following to the `.travis.yml` file in the root directory of your blog:

```yaml
sudo: false
language: c
branches:
  only:
  - source
install:
- mkdir -p ~/.local/bin
- export PATH=$HOME/.local/bin:$PATH
- travis_retry curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'
- stack --resolver lts --no-terminal --install-ghc install cabal-install happy alex
- stack --resolver lts --no-terminal install --fast
before_script:
- cd _site
- git checkout master
- git pull origin master
- git ls-files | xargs -r git rm
- cd ..
script:
- $HOME/.local/bin/site build
- cd _site
- git status
- git add --all
- git config --global user.email "abhinav@abhinavsarkar.net"
- git config --global user.name "Travis"
- git commit -m "$TRAVIS_COMMIT_MESSAGE"
- git push "$REPO_URL" master
- cd ..
cache:
  directories:
  - $HOME/.stack
  - $HOME/build/abhin4v/hakyll-staticman/.stack-work
```

Make sure to change the git `user.email` config to your email and `abhin4v/hakyll-staticman` in the cache section to your Github username and the blog's repo.

This file configures Travis CI to:

1. checkout the _source_ branch of the repo
1. install _stack_ and some required dependencies
1. build the site code
1. checkout and pull the _master_ branch of the repo in the `_site` directory
1. generate the static files for the site
1. add and commit all the generated files to the _master_ branch and push it to the repo
1. cache the built code and binaries for a faster build next time

Before pushing this file, head over to Travis CI to enable it for your repo. Sign in with your Github account and hit the **+** to add your repo.

![Adding the Repo to Travis CI](/images/static-comments-with-hakyll/travis-add-repo.png)

Flip the switch next to your repo's name to enable it and then click on small gear icon to go to the settings for your repo.

![Enabling the Repo](/images/static-comments-with-hakyll/travis-repo-settings.png)

In the "General" section, switch __on__ the "Build only if .travis.yml is present" button so that the pushes to the _master_ branch do not trigger builds. Also, switch __off__ the "Build pull request updates" button so that the comment PRs are not built (more about this in later sections).

![Configuring the General Settings](/images/static-comments-with-hakyll/travis-general-setting.png)

Now, go to the "Environment Variables" section and add an new variable called `REPO_URL`. The value of this variable should be the full URL to your blog's Github repo with your username and password. For my example blog, it looks like `https://abhin4v:<PASSWORD>@github.com/abhin4v/hakyll-staticman`. Replace `<PASSWORD>` with your Github password, making sure to escape any special characters for bash. Make sure that the "Display value in build log" button is set to __off__ so that your Github password is not exposed in the build logs.

![Adding the Repo URL Env Variable](/images/static-comments-with-hakyll/travis-env-variables.png)

Now go back and commit the `.travis.yml` file and push it to the repo. This should start a build in Travis CI which you should see in the "current" tab. Since this is the first build, it will take some time to download and build all the dependencies. Go ahead and grab a coffee (or your other favorite beverage).

Once your build is complete in Travis CI, you should be able to access your blog at `http://<GITHUB_USERNAME>.github.io/<REPO_NAME>`. In my case, this is <http://abhin4v.github.io/hakyll-staticman>[^customdomain].

[^source]: [Source of this blog](https://github.com/abhin4v/abhin4v.github.io/tree/source)
[^DSL]: [Domain specific language](https://en.wikipedia.org/wiki/Domain-specific_language)
[^CICD]: [Continuous Integration](https://en.wikipedia.org/wiki/Continuous_integration)/[Continuous Delivery](https://en.wikipedia.org/wiki/Continuous_delivery)
[^customdomain]: Don't be alarmed if you are redirected to <https://abhinavsarkar.net/hakyll-staticman/>. This is because I have a custom domain set for my blog hosted on Github.

[Hakyll]: https://jaspervdj.be/hakyll/
[Haskell]: http://haskell.org/
[Disqus]: https://disqus.com/
[Facebook]: https://developers.facebook.com/docs/plugins/comments
[Isso]: https://posativ.org/isso/
[Staticman]: https://staticman.net/
[_stack_]: http://www.haskellstack.org/

[1]: https://pages.github.com/
[2]: https://github.com/abhin4v/hakyll-staticman
[3]: http://travis-ci.org/
