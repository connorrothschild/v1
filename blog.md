---
permalink: /blog/
title: Blog
---

{% for post in site.posts %}
## [{{ post.title }}](v1/{{ post.url }})

*{{ post.date | date: '%B %d, %Y' }}*

{{ post.content | strip_html | truncatewords: 15 }} [Read more]({{ post.url }})
{% endfor %}
