module Bunpell where

open import Data.Product

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

data HasSuffix : Kana → Kana → Set where
  here : ∀ {tail} → HasSuffix tail tail
  there : ∀ {tail whole} (k : Kana → Kana) → HasSuffix whole tail → HasSuffix (k whole) tail

infixr 6 _▷_
_▷_ : ∀ {w t} (k : Kana → Kana) → HasSuffix w t → HasSuffix (k w) t
_▷_ = there

Suffix : Kana → Set
Suffix t = Σ Kana (λ w → HasSuffix w t)

-る : Set
-る = Suffix (る 。)

-く : Set
-く = Suffix (く 。)

-ぐ : Set
-ぐ = Suffix (ぐ 。)

-す : Set
-す = Suffix (す 。)

-つ : Set
-つ = Suffix (つ 。)

-ぬ : Set
-ぬ = Suffix (ぬ 。)

-ぶ : Set
-ぶ = Suffix (ぶ 。)

-む : Set
-む = Suffix (む 。)

-う : Set
-う = Suffix (う 。)

attach : ∀ {w t} → HasSuffix w t → Kana → Kana
attach here new = new
attach (there k p) new = k (attach p new)

suffix : ∀ {w t} → HasSuffix w t → Suffix t
suffix {w} p = w , p

data Verb : Set where
  Ichidan : -る → Verb
  Godan-K : -く → Verb
  Godan-G : -ぐ → Verb
  Godan-S : -す → Verb
  Godan-T : -つ → Verb
  Godan-N : -ぬ → Verb
  Godan-B : -ぶ → Verb
  Godan-M : -む → Verb
  Godan-R : -る → Verb
  Godan-W : -う → Verb

conjugate-verb : Style → Verb → Kana
conjugate-verb s (Ichidan (k , p)) with politeness s | tense s | mood s
... | Plain  | Past    | Positive = attach p (た 。)
... | Plain  | Past    | Negative = attach p (な か っ た 。)
... | Plain  | NonPast | Positive = k
... | Plain  | NonPast | Negative = attach p (な い 。)
... | Polite | Past    | Positive = attach p (ま し た 。)
... | Polite | Past    | Negative = attach p (ま せ ん で し た 。)
... | Polite | NonPast | Positive = attach p (ま す 。)
... | Polite | NonPast | Negative = attach p (ま せ ん 。)
conjugate-verb s (Godan-K (k , p)) with politeness s | tense s | mood s
... | Plain  | Past    | Positive = attach p (い た 。)
... | Plain  | Past    | Negative = attach p (か な か っ た 。)
... | Plain  | NonPast | Positive = k
... | Plain  | NonPast | Negative = attach p (か な い 。)
... | Polite | Past    | Positive = attach p (き ま し た 。)
... | Polite | Past    | Negative = attach p (き ま せ ん で し た 。)
... | Polite | NonPast | Positive = attach p (き ま す 。)
... | Polite | NonPast | Negative = attach p (き ま せ ん 。)
conjugate-verb s (Godan-G (k , p)) with politeness s | tense s | mood s
... | Plain  | Past    | Positive = attach p (い だ 。)
... | Plain  | Past    | Negative = attach p (が な か っ た 。)
... | Plain  | NonPast | Positive = k
... | Plain  | NonPast | Negative = attach p (が な い 。)
... | Polite | Past    | Positive = attach p (ぎ ま し た 。)
... | Polite | Past    | Negative = attach p (ぎ ま せ ん で し た 。)
... | Polite | NonPast | Positive = attach p (ぎ ま す 。)
... | Polite | NonPast | Negative = attach p (ぎ ま せ ん 。)
conjugate-verb s (Godan-S (k , p)) with politeness s | tense s | mood s
... | Plain  | Past    | Positive = attach p (し た 。)
... | Plain  | Past    | Negative = attach p (さ な か っ た 。)
... | Plain  | NonPast | Positive = k
... | Plain  | NonPast | Negative = attach p (さ な い 。)
... | Polite | Past    | Positive = attach p (し ま し た 。)
... | Polite | Past    | Negative = attach p (し ま せ ん で し た 。)
... | Polite | NonPast | Positive = attach p (し ま す 。)
... | Polite | NonPast | Negative = attach p (し ま せ ん 。)
conjugate-verb s (Godan-T (k , p)) with politeness s | tense s | mood s
... | Plain  | Past    | Positive = attach p (っ た 。)
... | Plain  | Past    | Negative = attach p (た な か っ た 。)
... | Plain  | NonPast | Positive = k
... | Plain  | NonPast | Negative = attach p (た な い 。)
... | Polite | Past    | Positive = attach p (ち ま し た 。)
... | Polite | Past    | Negative = attach p (ち ま せ ん で し た 。)
... | Polite | NonPast | Positive = attach p (ち ま す 。)
... | Polite | NonPast | Negative = attach p (ち ま せ ん 。)
conjugate-verb s (Godan-N (k , p)) with politeness s | tense s | mood s
... | Plain  | Past    | Positive = attach p (ん だ 。)
... | Plain  | Past    | Negative = attach p (な な か っ た 。)
... | Plain  | NonPast | Positive = k
... | Plain  | NonPast | Negative = attach p (な な い 。)
... | Polite | Past    | Positive = attach p (に ま し た 。)
... | Polite | Past    | Negative = attach p (に ま せ ん で し た 。)
... | Polite | NonPast | Positive = attach p (に ま す 。)
... | Polite | NonPast | Negative = attach p (に ま せ ん 。)
conjugate-verb s (Godan-B (k , p)) with politeness s | tense s | mood s
... | Plain  | Past    | Positive = attach p (ん だ 。)
... | Plain  | Past    | Negative = attach p (ば な か っ た 。)
... | Plain  | NonPast | Positive = k
... | Plain  | NonPast | Negative = attach p (ば な い 。)
... | Polite | Past    | Positive = attach p (び ま し た 。)
... | Polite | Past    | Negative = attach p (び ま せ ん で し た 。)
... | Polite | NonPast | Positive = attach p (び ま す 。)
... | Polite | NonPast | Negative = attach p (び ま せ ん 。)
conjugate-verb s (Godan-M (k , p)) with politeness s | tense s | mood s
... | Plain  | Past    | Positive = attach p (ん だ 。)
... | Plain  | Past    | Negative = attach p (ま な か っ た 。)
... | Plain  | NonPast | Positive = k
... | Plain  | NonPast | Negative = attach p (ま な い 。)
... | Polite | Past    | Positive = attach p (み ま し た 。)
... | Polite | Past    | Negative = attach p (み ま せ ん で し た 。)
... | Polite | NonPast | Positive = attach p (み ま す 。)
... | Polite | NonPast | Negative = attach p (み ま せ ん 。)
conjugate-verb s (Godan-R (k , p)) with politeness s | tense s | mood s
... | Plain  | Past    | Positive = attach p (っ た 。)
... | Plain  | Past    | Negative = attach p (ら な か っ た 。)
... | Plain  | NonPast | Positive = k
... | Plain  | NonPast | Negative = attach p (ら な い 。)
... | Polite | Past    | Positive = attach p (り ま し た 。)
... | Polite | Past    | Negative = attach p (り ま せ ん で し た 。)
... | Polite | NonPast | Positive = attach p (り ま す 。)
... | Polite | NonPast | Negative = attach p (り ま せ ん 。)
conjugate-verb s (Godan-W (k , p)) with politeness s | tense s | mood s
... | Plain  | Past    | Positive = attach p (っ た 。)
... | Plain  | Past    | Negative = attach p (わ な か っ た 。)
... | Plain  | NonPast | Positive = k
... | Plain  | NonPast | Negative = attach p (わ な い 。)
... | Polite | Past    | Positive = attach p (い ま し た 。)
... | Polite | Past    | Negative = attach p (い ま せ ん で し た 。)
... | Polite | NonPast | Positive = attach p (い ま す 。)
... | Polite | NonPast | Negative = attach p (い ま せ ん 。)

data Adjective : Set where
  い-adj : Adjective
  な-adj : Adjective

module Playground where
  taberu : -る
  taberu = suffix (た_ ▷ べ_ ▷ here)

  taberu-con : Kana
  taberu-con = conjugate-verb
    record { politeness = Polite; formality = Formal; tense = Past; mood = Negative }
    (Ichidan taberu)

  ex : Kana
  ex = わ た し の な ま え は ケ イ で す 。
