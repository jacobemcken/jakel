(ns jakel.include-support-test
  (:require [clojure.test :refer [deftest is]]
            [jakel.include-support :as sut]))

(deftest replace-deprecated-tag
  (is (=  "  {% if page.image %}
             {% render \"featured_image.html\", src: page.image, alt: page.image_alt %}
           {% endif %}

           {{ content }}"
         (sut/replace-deprecated-tag
          "  {% if page.image %}
             {% include featured_image.html
                src=page.image
                alt=page.image_alt %}
           {% endif %}

           {{ content }}"))))
