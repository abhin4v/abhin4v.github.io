---
title: About Me
page_type: profile
---
# About Me

I am Abhinav Sarkar. I'm a software engineer currently living in Bangalore, India. I currently work at [Google]. I've previously worked at [Flipkart], [nilenso software], [Capillary Technologies] and [FICO]. When not making software, I read books, play drums, listen to music and ride my bike.

Here are some talks and workshops I have presented in various conferences:

::: {.fancy-ul}
- [Clojure, Concurrency, and You] - In/Clojure 2019, Bangalore
- [Many Ways to Concur] - Functional Conf 2018, Bangalore
- [Moving People with Clojure] - EuroClojure 2017, Berlin
- [Introduction to Concurrency in Haskell] - Functional Conf 2015, Bangalore
- [A Slow (and Hopefully Heedful) Ride Through ReactJS and Flux][1] - JSFoo 2015, Bangalore
- [A Quick (and Hopefully Painless) Ride Through ReactJS][2] - Meta Refresh 2015, Bangalore
:::

You can see some projects I've done [here](/projects/).

You can find more about me on the internet:

::: {.fancy-ul}
- [LinkedIn]
- [StackOverflow]
- [Github]
:::

You can get in touch with me over [Twitter] or [email].

## About the Website

This is my personal website written and edited by me.

All content provided is for informational purposes only. The articles and posts on this website are my own and don’t necessarily represent the positions, strategies, or opinions of my employer or previous employers or their subsidiaries. I make no representations as to the accuracy or completeness of any information found here or by following any links. I will not be liable for any errors or omissions in this information nor for the availability of this information. I will not be liable for any losses, injuries, or damages from the display or use of this information.

The source of the website can be found on my [github][3]. The [master] branch contains the static website which is generated from the code and the content in the [source] branch.

It uses the [Hakyll] library as the base for static website generation, along with [Staticman] for the static comments support. Website styling is done with [SCSS] and interactivity is added using [JQuery]. The PDF files are generated using headless [Google Chrome] browser with [this tool] running on [node.js].

The website is run using the [hastatic] server for [docker]. It is hosted on [Digital Ocean], behind [Cloudflare] for caching. Builds are done automatically using a self-hosted [Drone CI] instance.

![CI pipeline of abhinavsarkar.net](https://notes.abhinavsarkar.net/files/site-pipeline/site.svg)

### Sitemap

::: {.fancy-ul}
- Core
  - [Home](/)
  - [About](/about/)
  - [Now](/now/)
- Writing
  - [Posts](/archive/)
  - [Notes](/notes/)
- Other
  - [Projects](/projects/)
  - [Activities](/activities/)
  - [Photos](/photos/)
  - [Readings](/readings/)
:::

[LinkedIn]: http://in.linkedin.com/in/abhinavsarkar
[StackOverflow]: https://stackoverflow.com/story/abhinavsarkar
[Github]: https://github.com/abhin4v
[Goodreads]: http://www.goodreads.com/user/show/24614151-abhinav-sarkar
[Last.fm]: http://last.fm/user/abhin4v
[Strava]: http://www.strava.com/athletes/3485865
[Twitter]: https://twitter.com/abhin4v

[Google]: https://about.google
[Flipkart]: https://en.wikipedia.org/wiki/Flipkart
[nilenso software]: https://nilenso.com
[Capillary Technologies]: https://www.capillarytech.com
[FICO]: https://www.fico.com/

[Clojure, Concurrency, and You]: /talks/clojure-concurrency-you/
[Many Ways to Concur]: /talks/many-ways-to-concur/
[Moving People with Clojure]: /talks/moving-people-with-clojure/
[Introduction To Concurrency In Haskell]: /talks/intro-to-conc-in-haskell/
[1]: https://web.archive.org/web/20151024091258/https://jsfoo.in/2015/reactjs-workshop
[2]: https://web.archive.org/web/20160629192412/https://metarefresh.talkfunnel.com/2015/1326-a-quick-and-hopefully-painless-ride-through-reactj
[3]: https://github.com/abhin4v/abhin4v.github.io

[email]: mailto:abhinav@abhinavsarkar.net
[master]: https://github.com/abhin4v/abhin4v.github.io/tree/master
[source]: https://github.com/abhin4v/abhin4v.github.io/tree/source
[Hakyll]: https://jaspervdj.be/hakyll/
[Staticman]: http://staticman.net/
[SCSS]: http://sass-lang.com/
[JQuery]: http://jquery.com/
[docker]: http://docker.com/
[hastatic]: https://github.com/abhin4v/hastatic/
[Digital Ocean]: https://www.digitalocean.com/
[Cloudflare]: https://www.cloudflare.com/
[Drone CI]: https://drone.io/
[this tool]: https://github.com/Szpadel/chrome-headless-render-pdf
[Google Chrome]: https://www.google.com/chrome/index.html
[node.js]: https://nodejs.org/
