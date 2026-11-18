# config/

Plain CSV tables read by `global.R` at start-up. They can be edited directly (Excel, LibreOffice):
no code change is needed. The CSVs hold only a header row and data rows - all explanations live
here, so the files open cleanly in a spreadsheet. Save as CSV with comma separator.

## sml_scenario_variables.csv

Source of truth for the Scenario Testing tab (Soil Monitoring Law section): one row per SML
Annex I variable.

| Column | Meaning |
|---|---|
| `id` | Short identifier. Also the column name expected in the sample upload CSV. |
| `label` | Name shown in the app. |
| `unit` | Unit shown in the app. |
| `iso` | Reference method (ISO or other standard) for measuring the variable. To be completed - see GitHub issue #5. |
| `part` | SML Annex I part the variable belongs to (A or B). |
| `direction` | `below`: healthy when value <= threshold. `above`: healthy when value >= threshold. |
| `fixed_value` | EU-set threshold, Part A only (same for every scenario). Empty for Part B: there the threshold is entered per scenario in the app. |
| `minotaur_column` | Matching column in the raw MINOTAUR data, used by "Use current MINOTAUR samples selection". Empty where there is no single matching column. `soc_clay` is empty but still supported: it is computed from `soc_mean` and texture (sand, silt) in `server.R`, `minotaur_samples_for_sml()`. |
| `plausible_min` / `plausible_max` | Draft range used only for the upload warning that flags likely unit mistakes. Not a legal or scientific threshold - rough figures pending review by colleagues. `soc_clay` max widened from 1 to 10 on 2026-10-14 after real MINOTAUR sites (sandy/silty soils) showed genuine ratios up to about 5.2. |
| `envo_iri` / `agrovoc_iri` | Ontology terms for the variable (ENVO, AGROVOC). Looked up on 2026-10-19 on the OLS/EBI and AGROVOC services; left empty where no real match exists. |

Notes on the ontology mapping:

- Several matches are approximate: `erosion` maps to the process "soil erosion", not to a rate;
  `bulk_density` maps to the generic AGROVOC "soil density"; `ksat` to the generic "hydraulic
  conductivity"; `air_capacity` to the broader "aeration".
- `soc_clay` is a ratio with no term in either vocabulary. Its parts exist in AGROVOC ("soil
  organic carbon" c_389fe908, "clay" c_15619) but are not used, since neither alone is the ratio.
- `soc_stock` has no match. AGROVOC "carbon sequestration" (c_331583) is a process, not a stock,
  and is deliberately not used.

## sml_national_precedent.csv

Draft dataset from an AI-assisted legislative scan (2026-09-29 to 2026-10-02), **not yet verified**
by a person. One row per SML Annex I variable x Member State x legislative finding. Shown in the
app's SML Annex I table.

| Column | Meaning |
|---|---|
| `variable_id` | Matches `id` in `sml_scenario_variables.csv`. |
| `country` | Member State. |
| `status` | `found`: official source fetched and read. `uncertain`: candidate found but not fully verified. |
| `instrument` | Name of the national legal act. |
| `legal_category` | Type of law (e.g. waste / landfill law). |
| `source_link` | Link to the official source; may be empty. |

Numeric thresholds are deliberately left out: units, extraction methods and quantities differ too
much between countries to show them as comparable numbers.

## sml_member_states.csv

Information collected over time on each EU Member State, shown in the "Member State profile" card
at the bottom of the SML tab (issue #6). One row per Member State. Empty cells are shown in the
app as "not collected yet", so the file can be filled in gradually. Only enter information checked
on an official source, and record that source.

| Column | Meaning |
|---|---|
| `country` | Member State name. Must match `country` in `sml_national_precedent.csv`. |
| `iso2` | ISO 3166-1 alpha-2 code (Greece is `GR`, not the EU's `EL`). Used for the flag. |
| `monitoring_network` | Name of the national soil (health) monitoring network, if any. |
| `monitoring_network_link` | Link to the network's official page. |
| `competent_authority` | National competent authority designated for the Soil Monitoring Law. |
| `competent_authority_link` | Link to the authority's page or to the act that designates it. |
| `source` | Where the information above was checked (link or reference). |
| `last_checked` | Date of the check, as YYYY-MM-DD. |
| `notes` | Anything else worth showing (shown as a short note in the card). |

The card's list of SML parameters touched by national legislation is not stored here: it comes
from `sml_national_precedent.csv`.
