# Codebook: `annotations.csv`

One row per judgment: one rater's answers about one segment in one
model's translation into the rater's locale. 54,600 rows. The design has
104 segments × 7 models × 75 raters = 54,600 judgments.

| Column | Description |
|---|---|
| `segment_id` | Segment identifier: source email and segment number, e.g. `email_3_13`. Unique for each of the 104 segments. |
| `segment_english` | The English source text of the segment. Two different segments share the text `scent`, so use `segment_id`, not this column, to identify segments. |
| `email_number` | Source email, `email_1` to `email_5`. |
| `model` | LLM that produced the translation, as recorded during data collection: `Claude Sonnet 4`, `Cohere Aya Expanse 8B`, `Deepseek V3.1`, `GPT-5`, `gpt-oss 120b`, `Llama 4`, `Mistral Medium 3.1`. The paper writes `DeepSeek V3.1` and `gpt-oss 120B`. |
| `language` | Target locale: Afrikaans, Arabic, Brazilian Portuguese, Cantonese, Czech, Dutch, Hebrew, Hindi, Japanese, Korean, Mandarin, Russian, Spanish, Swahili, Urdu. Region codes are listed in the paper (Section 3.1). |
| `rater_id` | Anonymous rater ID, `R01` to `R75`. Each rater judged only their own locale (five raters per locale). |
| `segment_quality` | Whether the segment was translated and, if so, how well. See below. |
| `best_if_translated` | Follow-up answer for translated segments. See below. |
| `best_if_english` | Follow-up answer for segments left in English. See below. |

## Response codes

**`segment_quality`**

| Code | Rating |
|---|---|
| `not_translated` | The segment was left in English (retained) |
| `serious_failures` | 1: serious failures exist |
| `imperfect_not_terrible` | 2: imperfect but not terrible |
| `mostly_good_small_issues` | 3: mostly good with small issues |
| `very_good_nearly_perfect` | 4: very good or nearly perfect |

**`best_if_translated`** (asked when the segment was translated)

| Code | Answer |
|---|---|
| `fully_translated_best` | This is the best translation |
| `partially_translated_best` | The text was only partially translated, but this was the best possible translation |
| `acceptable_better_exists` | This translation is acceptable, but a better translation exists |
| `unacceptable_translation` | This translation would not be acceptable |

**`best_if_english`** (asked when the segment was left in English)

| Code | Answer |
|---|---|
| `english_culturally_appropriate` | Translations exist, but English is the most culturally appropriate choice in this context |
| `no_good_translation_english_best` | There is no good translation, so English is the best choice |
| `good_translations_exist_english_hurts` | Good translations exist, and using English reduces the quality of the translation |


