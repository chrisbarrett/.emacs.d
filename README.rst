.. |travis| image:: https://travis-ci.org/proofit404/company-tern.png
    :target: https://travis-ci.org/proofit404/company-tern
    :alt: Build Status

.. |coveralls| image:: https://coveralls.io/repos/proofit404/company-tern/badge.png
    :target: https://coveralls.io/r/proofit404/company-tern
    :alt: Coverage Status

.. |melpa| image:: http://melpa.org/packages/company-tern-badge.svg
    :target: http://melpa.org/#/company-tern
    :alt: Melpa

.. |melpa-stable| image:: http://stable.melpa.org/packages/company-tern-badge.svg
    :target: http://stable.melpa.org/#/company-tern
    :alt: Melpa Stable

============
Company tern
============

|travis| |coveralls| |melpa| |melpa-stable|

Tern_ backend for company-mode_.

Installation
------------

You can install this package from Melpa_::

    M-x package-install RET company-tern RET

Usage
-----

Add ``company-tern`` to allowed ``company-mode`` backends list

.. code:: lisp

    (add-to-list 'company-backends 'company-tern)

If you don't like circles after an object's own properties, consider a
less annoying marker for that purpose or disable it entirely.

.. code:: lisp

    (setq company-tern-property-marker " <p>")
    (setq company-tern-property-marker nil)

You can trim too long function signatures to the frame width.

.. code:: lisp

    (setq company-tern-meta-as-single-line t)

If you doesn't like inline argument annotations appear with
corresponding identifiers, then you can to set up the company align
option.

.. code:: lisp

    (setq company-tooltip-align-annotations t)

Thanks
------

* **@katspaugh**
* **@dgutov**

.. _Tern: http://ternjs.net/
.. _company-mode: http://company-mode.github.io/
.. _Melpa: http://melpa.milkbox.net/
