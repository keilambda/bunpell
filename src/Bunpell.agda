module Bunpell where

data Kana : Set where
  あ_ い_ う_ え_ お_ : Kana → Kana
  か_ き_ く_ け_ こ_ : Kana → Kana
  さ_ し_ す_ せ_ そ_ : Kana → Kana
  た_ ち_ つ_ て_ と_ : Kana → Kana
  な_ に_ ぬ_ ね_ の_ : Kana → Kana
  は_ ひ_ ふ_ へ_ ほ_ : Kana → Kana
  ま_ み_ む_ め_ も_ : Kana → Kana
  や_ ゆ_ よ_ : Kana → Kana
  ら_ り_ る_ れ_ ろ_ : Kana → Kana
  わ_ を_ ん_ : Kana → Kana

  が_ ぎ_ ぐ_ げ_ ご_ : Kana → Kana
  ざ_ じ_ ず_ ぜ_ ぞ_ : Kana → Kana
  だ_ ぢ_ づ_ で_ ど_ : Kana → Kana
  ば_ び_ ぶ_ べ_ ぼ_ : Kana → Kana
  ぱ_ ぴ_ ぷ_ ぺ_ ぽ_ : Kana → Kana

  ゃ_ ゅ_ ょ_ : Kana → Kana

  っ_ : Kana → Kana

  、_ : Kana → Kana
  。 : Kana
