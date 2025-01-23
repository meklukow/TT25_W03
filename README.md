# TT25_W03
Tidy Tuesday Himalaya project for week 3 in 2025

Thank you to Nicole Foss, Ed.D., MS for this week's data!

I chose to create plots as if someone were interested in climbing the highest peak in each of the Himalayan ranges, including plots of risks.

Data Dictionary

exped_tidy.csv

variable	class	description
EXPID	factor	Expedition ID.
PEAKID	factor	Peak ID
YEAR	factor	Year of the expedition.
SEASON	integer	Season of the expedition as an integer.
SEASON_FACTOR	character	Season of the expedition converted to a the descriptive name.
HOST	integer	Host county of the expedition as an integer.
HOST_FACTOR	character	Host country of the expedition converted to a descriptive name.
ROUTE1	factor	Climbing route 1.
ROUTE2	factor	Climbing route 2.
ROUTE3	factor	Climbing route 3.
ROUTE4	factor	Climbing route 4.
NATION	factor	Principle nationality.
LEADERS	factor	Leadership of the expedition.
SPONSOR	factor	Expedition sponsor / name.
SUCCESS1	logical	Success on route 1?
SUCCESS2	logical	Success on route 2?
SUCCESS3	logical	Success on route 3?
SUCCESS4	logical	Success on route 4?
ASCENT1	factor	Ascent numbers for route 1.
ASCENT2	factor	Ascent numbers for route 2.
ASCENT3	factor	Ascent numbers for route 3.
ASCENT4	factor	Ascent numbers for route 4.
CLAIMED	logical	Success claimed?
DISPUTED	logical	Success disputed?
COUNTRIES	factor	Other countries.
APPROACH	factor	Approach march.
BCDATE	date	Date arrived at base camp.
SMTDATE	date	Date reached summit.
SMTTIME	factor	Time reached summit.
SMTDAYS	integer	Number of days to summit / high-point.
TOTDAYS	integer	Total number of days.
TERMDATE	date	Date the expedition was terminated.
TERMREASON	integer	Reason the expedition was terminated as an integer.
TERMREASON_FACTOR	character	Reason the expedition was terminated converted to a descriptive name.
TERMNOTE	factor	Termination details.
HIGHPOINT	integer	Expedition high-point.
TRAVERSE	logical	Did the expedition traverse.
SKI	logical	Ski / snowboard descent?
PARAPENTE	logical	Parapente descent?
CAMPS	integer	Number of high camps above base camp.
ROPE	integer	Amount of fixed rope.
TOTMEMBERS	integer	Number of members.
SMTMEMBERS	integer	Number of members on summit.
MDEATHS	integer	Number of member deaths.
TOTHIRED	integer	Number of hired personnel (above base camp).
SMTHIRED	integer	Number of hired personnel on summit.
HDEATHS	integer	Number of hired personnel deaths.
NOHIRED	logical	No hired personnel used above base camp.
O2USED	logical	Oxygen used?
O2NONE	logical	Oxygen not used?
O2CLIMB	logical	Oxygen climbing?
O2DESCENT	logical	Oxygen descending?
O2SLEEP	logical	Oxygen sleeping?
O2MEDICAL	logical	Oxygen used medically?
O2TAKEN	logical	Oxygen taken, not used?
O2UNKWN	logical	Oxygen use unknown?
OTHERSMTS	factor	Other summits.
CAMPSITES	factor	Campsite details.
ROUTEMEMO	factor	Route details.
ACCIDENTS	factor	Accidents on the expedition.
ACHIEVMENT	factor	Achievements.
AGENCY	factor	Trekking agency.
COMRTE	logical	Commercial route?
STDRTE	logical	8000m standard route?
PRIMRTE	logical	Route info with primary expedition?
PRIMMEM	logical	Member info with primary expedition?
PRIMREF	logical	Literature info with primary expedition?
PRIMID	factor	Primary expedition ID (if any).
CHKSUM	integer	Internal consistency check.
peaks_tidy.csv

variable	class	description
PEAKID	integer	Peak ID, the key field (unique identifier) for each record.
PKNAME	integer	Peak name.
PKNAME2	integer	Peak name 2.
LOCATION	integer	Location of the peak.
HEIGHTM	integer	Peak height in meters.
HEIGHTF	integer	Peak height in feet.
HIMAL	integer	Mountain range identifier. See HIMAL_FACTOR for names.
HIMAL_FACTOR	integer	Mountain range name.
REGION	integer	Region identifier.
REGION_FACTOR	character	Region name.
OPEN	logical	Indicates whether the peak is open for expeditions.
UNLISTED	logical	Indicates whether the peak is unlisted in official records.
TREKKING	logical	Indicates whether the peak is a trekking peak.
TREKYEAR	integer	Year the peak was designated as a trekking peak.
RESTRICT	logical	Indicates whether the peak is restricted for climbing or access.
PHOST	integer	Primary host country identifier for the peak. See PHOST_FACTOR for names.
PHOST_FACTOR	character	Primary host country name corresponding to the PHOST identifier.
PSTATUS	integer	Climbing status identifier for the peak. See PSTATUS_FACTOR for names.
PSTATUS_FACTOR	character	Climbing status description corresponding to the PSTATUS identifier.
PEAKMEMO	character	Additional notes or remarks about the peak.
PYEAR	integer	Year of the first recorded climbing attempt on the peak.
PSEASON	character	Climbing season associated with the first recorded ascent or expedition.
PEXPID	integer	Expedition ID associated with the first recorded climb.
PSMTDATE	character	Date of the first successful summit.
PCOUNTRY	character	Country of origin for the expedition or climbers.
PSUMMITERS	integer	Number of climbers who first reached the summit.
PSMTNOTE	character	Notes or remarks related to the first successful summit.
REFERMEMO	character	Peak chronology references.
PHOTOMEMO	character	Peak photo references.
