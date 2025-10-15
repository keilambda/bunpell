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

  ア_ イ_ ウ_ エ_ オ_ : Kana → Kana
  カ_ キ_ ク_ ケ_ コ_ : Kana → Kana
  サ_ シ_ ス_ セ_ ソ_ : Kana → Kana
  タ_ チ_ ツ_ テ_ ト_ : Kana → Kana
  ナ_ ニ_ ヌ_ ネ_ ノ_ : Kana → Kana
  ハ_ ヒ_ フ_ ヘ_ ホ_ : Kana → Kana
  マ_ ミ_ ム_ メ_ モ_ : Kana → Kana
  ヤ_ ユ_ ヨ_ : Kana → Kana
  ラ_ リ_ ル_ レ_ ロ_ : Kana → Kana
  ワ_ ヲ_ ン_ : Kana → Kana

  ガ_ ギ_ グ_ ゲ_ ゴ_ : Kana → Kana
  ザ_ ジ_ ズ_ ゼ_ ゾ_ : Kana → Kana
  ダ_ ヂ_ ヅ_ デ_ ド_ : Kana → Kana
  バ_ ビ_ ブ_ ベ_ ボ_ : Kana → Kana
  パ_ ピ_ プ_ ペ_ ポ_ : Kana → Kana

  ャ_ ュ_ ョ_ : Kana → Kana

  ッ_ : Kana → Kana

  ヴ_ : Kana → Kana
  ァ_ ィ_ ゥ_ ェ_ ォ_ : Kana → Kana
  ヵ_ ヶ_ : Kana → Kana

  、_ : Kana → Kana
  。 : Kana

data Formality : Set where
  Casual : Formality
  Formal : Formality

data Politeness : Set where
  Plain : Politeness
  Polite : Politeness

data Tense : Set where
  Past : Tense
  NonPast : Tense

data Mood : Set where
  Positive : Mood
  Negative : Mood

record Style : Set where
  field
    politeness : Politeness
    formality : Formality
    tense : Tense
    mood : Mood

data Verb : Set where
  Ichidan : Verb
  Godan : Verb

data Adjective : Set where
  い-adj : Adjective
  な-adj : Adjective

ex : Kana
ex = わ た し の な ま え は ケ イ で す 。
