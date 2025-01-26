(ns jakel.core-test
  (:require [clojure.test :refer [deftest is]]
            [jakel :as sut]))

(deftest replace-include-syntax
  (is (=  "  {% if page.image %}
             {% render \"featured_image.html\", src: page.image, alt: page.image_alt %}
           {% endif %}

           {{ content }}"
         (sut/replace-include-syntax
          "  {% if page.image %}
             {% include featured_image.html
                src=page.image
                alt=page.image_alt %}
           {% endif %}

           {{ content }}"))))
