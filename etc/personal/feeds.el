(setq elfeed-feeds
      '(;; humour
        ("http://xkcd.com/rss.xml"  humour)
        ("http://wdr1.com/blog/calvin_and_hobbes.rdf" humour)
        ;; emacs
        ("http://planet.emacsen.org/atom.xml" emacs)
        ("http://www.masteringemacs.org/feed/" emacs)
        ;; raspberry pi
        ("https://osmc.tv/feed/" kodi osmc)
        ("https://github.com/moneymaker365/plugin.video.ustvvod/commits/master.atom" osmc kodi ustvvod github)
        ))

(provide 'feeds)
