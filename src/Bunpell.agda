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

open Style

data HasEnding : Kana → Kana → Set where
  here : ∀ {tail} → HasEnding tail tail
  there : ∀ {tail whole} (k : Kana → Kana) → HasEnding whole tail → HasEnding (k whole) tail

infixr 6 _▷_
_▷_ : ∀ {w t} (k : Kana → Kana) → HasEnding w t → HasEnding (k w) t
_▷_ = there

infix 4 _-る
_-る : Kana → Set
k -る = HasEnding k (る 。)

infix 4 _-く
_-く : Kana → Set
k -く = HasEnding k (く 。)

infix 4 _-ぐ
_-ぐ : Kana → Set
k -ぐ = HasEnding k (ぐ 。)

infix 4 _-す
_-す : Kana → Set
k -す = HasEnding k (す 。)

infix 4 _-つ
_-つ : Kana → Set
k -つ = HasEnding k (つ 。)

infix 4 _-ぬ
_-ぬ : Kana → Set
k -ぬ = HasEnding k (ぬ 。)

infix 4 _-ぶ
_-ぶ : Kana → Set
k -ぶ = HasEnding k (ぶ 。)

infix 4 _-む
_-む : Kana → Set
k -む = HasEnding k (む 。)

infix 4 _-う
_-う : Kana → Set
k -う = HasEnding k (う 。)

attach : ∀ {w t} → HasEnding w t → Kana → Kana
attach here new = new
attach (there k p) new = k (attach p new)

data Verb : Set where
  Ichidan : (k : Kana) → k -る → Verb
  Godan-K : (k : Kana) → k -く → Verb
  Godan-G : (k : Kana) → k -ぐ → Verb
  Godan-S : (k : Kana) → k -す → Verb
  Godan-T : (k : Kana) → k -つ → Verb
  Godan-N : (k : Kana) → k -ぬ → Verb
  Godan-B : (k : Kana) → k -ぶ → Verb
  Godan-M : (k : Kana) → k -む → Verb
  Godan-R : (k : Kana) → k -る → Verb
  Godan-W : (k : Kana) → k -う → Verb

conjugate-verb : Style → Verb → Kana
conjugate-verb s (Ichidan k p) with politeness s | tense s | mood s
... | Plain  | Past    | Positive = attach p (た 。)
... | Plain  | Past    | Negative = attach p (な か っ た 。)
... | Plain  | NonPast | Positive = k
... | Plain  | NonPast | Negative = attach p (な い 。)
... | Polite | Past    | Positive = attach p (ま し た 。)
... | Polite | Past    | Negative = attach p (ま せ ん で し た 。)
... | Polite | NonPast | Positive = attach p (ま す 。)
... | Polite | NonPast | Negative = attach p (ま せ ん 。)
conjugate-verb s (Godan-K k p) with politeness s | tense s | mood s
... | Plain  | Past    | Positive = attach p (い た 。)
... | Plain  | Past    | Negative = attach p (か な か っ た 。)
... | Plain  | NonPast | Positive = k
... | Plain  | NonPast | Negative = attach p (か な い 。)
... | Polite | Past    | Positive = attach p (き ま し た 。)
... | Polite | Past    | Negative = attach p (き ま せ ん で し た 。)
... | Polite | NonPast | Positive = attach p (き ま す 。)
... | Polite | NonPast | Negative = attach p (き ま せ ん 。)
conjugate-verb s (Godan-G k p) with politeness s | tense s | mood s
... | Plain  | Past    | Positive = attach p (い だ 。)
... | Plain  | Past    | Negative = attach p (が な か っ た 。)
... | Plain  | NonPast | Positive = k
... | Plain  | NonPast | Negative = attach p (が な い 。)
... | Polite | Past    | Positive = attach p (ぎ ま し た 。)
... | Polite | Past    | Negative = attach p (ぎ ま せ ん で し た 。)
... | Polite | NonPast | Positive = attach p (ぎ ま す 。)
... | Polite | NonPast | Negative = attach p (ぎ ま せ ん 。)
conjugate-verb s (Godan-S k p) with politeness s | tense s | mood s
... | Plain  | Past    | Positive = attach p (し た 。)
... | Plain  | Past    | Negative = attach p (さ な か っ た 。)
... | Plain  | NonPast | Positive = k
... | Plain  | NonPast | Negative = attach p (さ な い 。)
... | Polite | Past    | Positive = attach p (し ま し た 。)
... | Polite | Past    | Negative = attach p (し ま せ ん で し た 。)
... | Polite | NonPast | Positive = attach p (し ま す 。)
... | Polite | NonPast | Negative = attach p (し ま せ ん 。)
conjugate-verb s (Godan-T k p) with politeness s | tense s | mood s
... | Plain  | Past    | Positive = attach p (っ た 。)
... | Plain  | Past    | Negative = attach p (た な か っ た 。)
... | Plain  | NonPast | Positive = k
... | Plain  | NonPast | Negative = attach p (た な い 。)
... | Polite | Past    | Positive = attach p (ち ま し た 。)
... | Polite | Past    | Negative = attach p (ち ま せ ん で し た 。)
... | Polite | NonPast | Positive = attach p (ち ま す 。)
... | Polite | NonPast | Negative = attach p (ち ま せ ん 。)
conjugate-verb s (Godan-N k p) with politeness s | tense s | mood s
... | Plain  | Past    | Positive = attach p (ん た 。)
... | Plain  | Past    | Negative = attach p (な な か っ た 。)
... | Plain  | NonPast | Positive = k
... | Plain  | NonPast | Negative = attach p (な な い 。)
... | Polite | Past    | Positive = attach p (な ま し た 。)
... | Polite | Past    | Negative = attach p (な ま せ ん で し た 。)
... | Polite | NonPast | Positive = attach p (な ま す 。)
... | Polite | NonPast | Negative = attach p (な ま せ ん 。)
conjugate-verb s (Godan-B k p) = 。
conjugate-verb s (Godan-M k p) = 。
conjugate-verb s (Godan-R k p) with politeness s | tense s | mood s
... | Plain  | Past    | Positive = attach p (っ た 。)
... | Plain  | Past    | Negative = attach p (ら な か っ た 。)
... | Plain  | NonPast | Positive = k
... | Plain  | NonPast | Negative = attach p (ら な い 。)
... | Polite | Past    | Positive = attach p (り ま し た 。)
... | Polite | Past    | Negative = attach p (り ま せ ん で し た 。)
... | Polite | NonPast | Positive = attach p (り ま す 。)
... | Polite | NonPast | Negative = attach p (り ま せ ん 。)
conjugate-verb s (Godan-W k p) with politeness s | tense s | mood s
... | Plain  | Past    | Positive = attach p (っ た 。)
... | Plain  | Past    | Negative = attach p (わ な か っ た 。)
... | Plain  | NonPast | Positive = k
... | Plain  | NonPast | Negative = attach p (わ な い 。)
... | Polite | Past    | Positive = attach p (わ ま し た 。)
... | Polite | Past    | Negative = attach p (わ ま せ ん で し た 。)
... | Polite | NonPast | Positive = attach p (わ ま す 。)
... | Polite | NonPast | Negative = attach p (わ ま せ ん 。)

data Adjective : Set where
  い-adj : Adjective
  な-adj : Adjective

module Playground where
  taberu : Kana
  taberu = た べ る 。

  taberu-ending : taberu -る
  taberu-ending = た_ ▷ べ_ ▷ here

  oki-con : Kana
  oki-con = conjugate-verb
    record { politeness = Polite; formality = Formal; tense = Past; mood = Negative }
    (Ichidan taberu taberu-ending)

  ex : Kana
  ex = わ た し の な ま え は ケ イ で す 。
