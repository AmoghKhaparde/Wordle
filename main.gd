extends Node2D

var WORD = "rouge"
var userGuess = "" # guess of the user
var letterOccurences = {} # number of times each letter has occured

# list of all words used and allowed in the game
var wordList = [
	"aback", "abase", "abate", "abbey", "abhor", "abide", "abled", "abode", "abort",
	"about", "above", "abuse", "abyss", "acorn", "acrid", "actor", "acute", "adage", "adapt",
	"adept", "admin", "admit", "adobe", "adopt", "adore", "adorn", "adult", "affix", "afire",
	"afoot", "afoul", "after", "again", "agape", "agate", "agent", "agile", "aging", "aglow",
	"agony", "agora", "agree", "ahead", "aider", "aisle", "alarm", "album", "alert", "algae",
	"alibi", "alien", "align", "alike", "alive", "allay", "alley", "allot", "allow", "alloy",
	"aloft", "alone", "along", "aloof", "aloud", "alpha", "altar", "alter", "amass", "amaze",
	"amber", "amble", "amend", "amiss", "amity", "among", "ample", "amply", "amuse", "angel",
	"anger", "angle", "angry", "angst", "anime", "ankle", "annex", "annoy", "annul", "anode",
	"antic", "anvil", "aorta", "apart", "aphid", "aping", "apnea", "apple", "apply", "apron",
	"aptly", "arbor", "ardor", "arena", "argue", "arise", "armor", "aroma", "arose", "array",
	"arrow", "arson", "artsy", "ascot", "ashen", "aside", "askew", "assay", "asset", "atoll",
	"atone", "attic", "audio", "audit", "augur", "aunty", "avail", "avert", "avian", "avoid",
	"await", "awake", "award", "aware", "awash", "awful", "awoke", "axial", "axiom", "axion",
	"azure", "bacon", "badge", "badly", "bagel", "baggy", "baker", "baler", "balmy", "banal",
	"banjo", "barge", "baron", "basal", "basic", "basil", "basin", "basis", "baste", "batch",
	"bathe", "baton", "batty", "bawdy", "bayou", "beach", "beady", "beard", "beast", "beech",
	"beefy", "befit", "began", "begat", "beget", "begin", "begun", "being", "belch", "belie",
	"belle", "belly", "below", "bench", "beret", "berry", "berth", "beset", "betel", "bevel",
	"bezel", "bible", "bicep", "biddy", "bigot", "bilge", "billy", "binge", "bingo", "biome",
	"birch", "birth", "bison", "bitty", "black", "blade", "blame", "bland", "blank", "blare",
	"blast", "blaze", "bleak", "bleat", "bleed", "bleep", "blend", "bless", "blimp", "blind",
	"blink", "bliss", "blitz", "bloat", "block", "bloke", "blond", "blood", "bloom", "blown",
	"bluer", "bluff", "blunt", "blurb", "blurt", "blush", "board", "boast", "bobby", "boney",
	"bongo", "bonus", "boost", "booth", "booty", "booze", "boozy", "borax", "borne",
	"bosom", "bossy", "botch", "bough", "boule", "bound", "bowel", "boxer", "brace", "braid",
	"brain", "brake", "brand", "brash", "brass", "brave", "bravo", "brawl", "brawn", "bread",
	"break", "breed", "briar", "bribe", "brick", "bride", "brief", "brine", "bring", "brink",
	"briny", "brisk", "broad", "broil", "broke", "brood", "brook", "broom", "broth", "brown",
	"brunt", "brush", "brute", "buddy", "budge", "buggy", "bugle", "build", "built", "bulge",
	"bulky", "bully", "bunch", "bunny", "burly", "burnt", "burst", "bused", "bushy", "butch",
	"butte", "buxom", "buyer", "bylaw", "cabal", "cabby", "cabin", "cable", "cacao", "cache",
	"cacti", "caddy", "cadet", "cagey", "cairn", "camel", "cameo", "canal", "candy", "canny",
	"canoe", "canon", "caper", "caput", "carat", "cargo", "carol", "carry", "carve", "caste",
	"catch", "cater", "catty", "caulk", "cause", "cavil", "cease", "cedar", "cello", "chafe",
	"chaff", "chain", "chair", "chalk", "champ", "chant", "chaos", "chard", "charm", "chart",
	"chase", "chasm", "cheap", "cheat", "check", "cheek", "cheer", "chess", "chest", "chick",
	"chide", "chief", "child", "chili", "chill", "chime", "china", "chirp", "chock", "choir",
	"choke", "chord", "chore", "chose", "chuck", "chump", "chunk", "churn", "chute", "cider",
	"cigar", "cinch", "circa", "civic", "civil", "clack", "claim", "clamp", "clang", "clank",
	"clash", "clasp", "class", "clean", "clear", "cleat", "cleft", "clerk", "click", "cliff",
	"climb", "cling", "clink", "cloak", "clock", "clone", "close", "cloth", "cloud", "clout",
	"clove", "clown", "cluck", "clued", "clump", "clung", "coach", "coast", "cobra", "cocoa",
	"colon", "color", "comet", "comfy", "comic", "comma", "conch", "condo", "conic", "copse",
	"coral", "corer", "corny", "couch", "cough", "could", "count", "coupe", "court", "coven",
	"cover", "covet", "covey", "cower", "coyly", "crack", "craft", "cramp", "crane", "crank",
	"crash", "crass", "crate", "crave", "crawl", "craze", "crazy", "creak", "cream", "credo",
	"creed", "creek", "creep", "creme", "crepe", "crept", "cress", "crest", "crick", "cried",
	"crier", "crime", "crimp", "crisp", "croak", "crock", "crone", "crony", "crook", "cross",
	"croup", "crowd", "crown", "crude", "cruel", "crumb", "crump", "crush", "crust", "crypt",
	"cubic", "cumin", "curio", "curly", "curry", "curse", "curve", "curvy", "cutie", "cyber",
	"cycle", "cynic", "daddy", "daily", "dairy", "daisy", "dally", "dance", "dandy", "datum",
	"daunt", "dealt", "death", "debar", "debit", "debug", "debut", "decal", "decay", "decor",
	"decoy", "decry", "defer", "deign", "deity", "delay", "delta", "delve", "demon", "demur",
	"denim", "dense", "depot", "depth", "derby", "deter", "detox", "deuce", "devil", "diary",
	"dicey", "digit", "dilly", "dimly", "diner", "dingo", "dingy", "diode", "dirge", "dirty",
	"disco", "ditch", "ditto", "ditty", "diver", "dizzy", "dodge", "dodgy", "dogma", "doing",
	"dolly", "donor", "donut", "dopey", "doubt", "dough", "dowdy", "dowel", "downy", "dowry",
	"dozen", "draft", "drain", "drake", "drama", "drank", "drape", "drawl", "drawn", "dread",
	"dream", "dress", "dried", "drier", "drift", "drill", "drink", "drive", "droit", "droll",
	"drone", "drool", "droop", "dross", "drove", "drown", "druid", "drunk", "dryer", "dryly",
	"duchy", "dully", "dummy", "dumpy", "dunce", "dusky", "dusty", "dutch", "duvet", "dwarf",
	"dwell", "dwelt", "dying", "eager", "eagle", "early", "earth", "easel", "eaten", "eater",
	"eclat", "edict", "edify", "eerie", "egret", "eight", "eject", "eking", "elate",
	"elbow", "elder", "elect", "elegy", "elfin", "elide", "elite", "elope", "elude", "email",
	"embed", "ember", "emcee", "empty", "enact", "endow", "enema", "enemy", "enjoy", "ennui",
	"ensue", "enter", "entry", "envoy", "epoch", "epoxy", "equal", "equip", "erase", "erect",
	"erode", "error", "erupt", "essay", "ester", "ether", "ethic", "ethos", "etude", "evade",
	"event", "every", "evict", "evoke", "exact", "exalt", "excel", "exert", "exile", "exist",
	"expel", "extol", "extra", "exult", "eying", "fable", "facet", "faint", "fairy", "faith",
	"false", "fancy", "fanny", "farce", "fatal", "fatty", "fault", "fauna", "favor", "feast",
	"feign", "fella", "felon", "femme", "femur", "fence", "feral", "ferry", "fetal",
	"fetch", "fetid", "fetus", "fever", "fewer", "fiber", "fibre", "ficus", "field", "fiend",
	"fiery", "fifth", "fifty", "fight", "filer", "filet", "filly", "filmy", "filth", "final",
	"finch", "finer", "first", "fishy", "fixer", "fizzy", "fjord", "flack", "flail", "flair",
	"flake", "flaky", "flame", "flank", "flare", "flash", "flask", "fleck", "fleet", "flesh",
	"flick", "flier", "fling", "flint", "flirt", "float", "flock", "flood", "floor", "flora",
	"floss", "flour", "flout", "flown", "fluff", "fluid", "fluke", "flume", "flung", "flunk",
	"flush", "flute", "flyer", "foamy", "focal", "focus", "foggy", "foist", "folio", "folly",
	"foray", "force", "forge", "forgo", "forte", "forth", "forty", "forum", "found", "foyer",
	"frail", "frame", "frank", "fraud", "freak", "freed", "freer", "fresh", "friar", "fried",
	"frill", "frisk", "fritz", "frock", "frond", "front", "frost", "froth", "frown", "froze",
	"fruit", "fudge", "fugue", "fully", "fungi", "funky", "funny", "furor", "furry", "fussy",
	"fuzzy", "gaffe", "gaily", "gamer", "gamma", "gamut", "gassy", "gaudy", "gauge", "gaunt",
	"gauze", "gavel", "gawky", "gayer", "gayly", "gazer", "gecko", "geeky", "geese", "genie",
	"genre", "ghost", "ghoul", "giant", "giddy", "gipsy", "girly", "girth", "given", "giver",
	"glade", "gland", "glare", "glass", "glaze", "gleam", "glean", "glide", "glint", "gloat",
	"globe", "gloom", "glory", "gloss", "glove", "glyph", "gnash", "gnome", "godly", "going",
	"golem", "golly", "gonad", "goner", "goody", "gooey", "goofy", "goose", "gorge", "gouge",
	"gourd", "grace", "grade", "graft", "grail", "grain", "grand", "grant", "grape", "graph",
	"grasp", "grass", "grate", "grave", "gravy", "graze", "great", "greed", "green", "greet",
	"grief", "grill", "grime", "grimy", "grind", "gripe", "groan", "groin", "groom", "grope",
	"gross", "group", "grout", "grove", "growl", "grown", "gruel", "gruff", "grunt", "guard",
	"guava", "guess", "guest", "guide", "guild", "guile", "guilt", "guise", "gulch", "gully",
	"gumbo", "gummy", "guppy", "gusto", "gusty", "gypsy", "habit", "hairy", "halve", "handy",
	"happy", "hardy", "harem", "harpy", "harry", "harsh", "haste", "hasty", "hatch", "hater",
	"haunt", "haute", "haven", "havoc", "hazel", "heady", "heard", "heart", "heath", "heave",
	"heavy", "hedge", "hefty", "heist", "helix", "hello", "hence", "heron", "hilly", "hinge",
	"hippo", "hippy", "hitch", "hoard", "hobby", "hoist", "holly", "homer", "honey", "honor",
	"horde", "horny", "horse", "hotel", "hotly", "hound", "house", "hovel", "hover", "howdy",
	"human", "humid", "humor", "humph", "humus", "hunch", "hunky", "hurry", "husky", "hussy",
	"hutch", "hydro", "hyena", "hymen", "hyper", "icily", "icing", "ideal", "idiom", "idiot",
	"idler", "idyll", "igloo", "iliac", "image", "imbue", "impel", "imply", "inane", "inbox",
	"incur", "index", "inept", "inert", "infer", "ingot", "inlay", "inlet", "inner", "input",
	"inter", "intro", "ionic", "irate", "irony", "islet", "issue", "itchy", "ivory", "jaunt",
	"jazzy", "jelly", "jerky", "jetty", "jewel", "jiffy", "joint", "joist", "joker", "jolly",
	"joust", "judge", "juice", "juicy", "jumbo", "jumpy", "junta", "junto", "juror", "kappa",
	"karma", "kayak", "kebab", "khaki", "kinky", "kiosk", "kitty", "knack", "knave", "knead",
	"kneed", "kneel", "knelt", "knife", "knock", "knoll", "known", "koala", "krill", "label",
	"labor", "laden", "ladle", "lager", "lance", "lanky", "lapel", "lapse", "large", "larva",
	"lasso", "latch", "later", "lathe", "latte", "laugh", "layer", "leach", "leafy", "leaky",
	"leant", "leapt", "learn", "lease", "leash", "least", "leave", "ledge", "leech", "leery",
	"lefty", "legal", "leggy", "lemon", "lemur", "leper", "level", "lever", "libel", "liege",
	"light", "liken", "lilac", "limbo", "limit", "linen", "liner", "lingo", "lipid", "lithe",
	"liver", "livid", "llama", "loamy", "loath", "lobby", "local", "locus", "lodge", "lofty",
	"logic", "login", "loopy", "loose", "lorry", "loser", "louse", "lousy", "lover", "lower",
	"lowly", "loyal", "lucid", "lucky", "lumen", "lumpy", "lunar", "lunch", "lunge", "lupus",
	"lurch", "lurid", "lusty", "lying", "lymph", "lynch", "lyric", "macaw", "macho", "macro",
	"madam", "madly", "mafia", "magic", "magma", "maize", "major", "maker", "mambo", "mamma",
	"mammy", "manga", "mange", "mango", "mangy", "mania", "manic", "manly", "manor", "maple",
	"march", "marry", "marsh", "mason", "masse", "match", "matey", "mauve", "maxim", "maybe",
	"mayor", "mealy", "meant", "meaty", "mecca", "medal", "media", "medic", "melee", "melon",
	"mercy", "merge", "merit", "merry", "messy", "metal", "meter", "metro", "micro", "midge", "midst",
	"might", "milky", "mimic", "mince", "miner", "minim", "minor", "minty", "minus", "mirth",
	"miser", "missy", "mocha", "modal", "model", "modem", "mogul", "moist", "molar", "moldy",
	"money", "month", "moody", "moose", "moral", "moron", "morph", "mossy", "motel", "motif",
	"motor", "motto", "moult", "mound", "mount", "mourn", "mouse", "mouth", "mover", "movie",
	"mower", "mucky", "mucus", "muddy", "mulch", "mummy", "munch", "mural", "murky", "mushy",
	"music", "musky", "musty", "myrrh", "nadir", "naive", "nanny", "nasal", "nasty", "natal",
	"naval", "navel", "needy", "neigh", "nerdy", "nerve", "never", "newer", "newly", "nicer",
	"niche", "niece", "night", "ninja", "ninny", "ninth", "noble", "nobly", "noise", "noisy",
	"nomad", "noose", "north", "nosey", "notch", "novel", "nudge", "nurse", "nutty", "nylon",
	"nymph", "oaken", "obese", "occur", "ocean", "octal", "octet", "odder", "oddly", "offal",
	"offer", "often", "olden", "older", "olive", "ombre", "omega", "onion", "onset", "opera",
	"opine", "opium", "optic", "orbit", "order", "organ", "other", "otter", "ought", "ounce",
	"outdo", "outer", "outgo", "ovary", "ovate", "overt", "ovine", "ovoid", "owing", "owner",
	"oxide", "ozone", "paddy", "pagan", "paint", "paler", "palsy", "panel", "panic", "pansy",
	"papal", "paper", "parer", "parka", "parry", "parse", "party", "pasta", "paste", "pasty",
	"patch", "patio", "patsy", "patty", "pause", "payee", "payer", "peace", "peach", "pearl",
	"pecan", "pedal", "penal", "pence", "penne", "penny", "perch", "peril", "perky", "pesky",
	"pesto", "petal", "petty", "phase", "phone", "phony", "photo", "piano", "picky", "piece",
	"piety", "piggy", "pilot", "pinch", "piney", "pinky", "pinto", "piper", "pique", "pitch",
	"pithy", "pivot", "pixel", "pixie", "pizza", "place", "plaid", "plain", "plait", "plane",
	"plank", "plant", "plate", "plaza", "plead", "pleat", "plied", "plier", "pluck", "plumb",
	"plume", "plump", "plunk", "plush", "poesy", "point", "poise", "poker", "polar", "polka",
	"polyp", "pooch", "poppy", "porch", "poser", "posit", "posse", "pouch", "pound", "pouty",
	"power", "prank", "prawn", "preen", "press", "price", "prick", "pride", "pried", "prime",
	"primo", "print", "prior", "prism", "privy", "prize", "probe", "prone", "prong", "proof",
	"prose", "proud", "prove", "prowl", "proxy", "prude", "prune", "psalm", "pubic", "pudgy",
	"puffy", "pulpy", "pulse", "punch", "pupal", "pupil", "puppy", "puree", "purer", "purge",
	"purse", "pushy", "putty", "pygmy", "quack", "quail", "quake", "qualm", "quark", "quart",
	"quash", "quasi", "queen", "queer", "quell", "query", "quest", "queue", "quick", "quiet",
	"quill", "quilt", "quirk", "quite", "quota", "quote", "quoth", "rabbi", "rabid", "racer",
	"radar", "radii", "radio", "rainy", "raise", "rajah", "rally", "ralph", "ramen", "ranch",
	"randy", "range", "rapid", "rarer", "raspy", "ratio", "ratty", "raven", "rayon", "razor",
	"reach", "react", "ready", "realm", "rearm", "rebar", "rebel", "rebus", "rebut", "recap",
	"recur", "recut", "reedy", "refer", "refit", "regal", "rehab", "reign", "relax", "relay",
	"relic", "remit", "renal", "renew", "repay", "repel", "reply", "rerun", "reset", "resin",
	"retch", "retro", "retry", "reuse", "revel", "revue", "rhino", "rhyme", "rider", "ridge",
	"rifle", "right", "rigid", "rigor", "rinse", "ripen", "riper", "risen", "riser", "risky",
	"rival", "river", "rivet", "roach", "roast", "robin", "robot", "rocky", "rodeo", "roger",
	"rogue", "roomy", "roost", "roots", "rotor", "rouge", "rough", "round", "rouse", "route", "rover",
	"rowdy", "rower", "royal", "ruddy", "ruder", "rugby", "ruler", "rumba", "rumor", "rupee",
	"rural", "rusty", "sadly", "safer", "saint", "salad", "sally", "salon", "salsa", "salty",
	"salve", "salvo", "sandy", "saner", "sappy", "sassy", "satin", "satyr", "sauce", "saucy",
	"sauna", "saute", "savor", "savoy", "savvy", "scald", "scale", "scalp", "scaly", "scamp",
	"scant", "scare", "scarf", "scary", "scene", "scent", "scion", "scoff", "scold", "scone",
	"scoop", "scope", "score", "scorn", "scour", "scout", "scowl", "scram", "scrap", "scree",
	"screw", "scrub", "scrum", "scuba", "sedan", "seedy", "segue", "seize", "semen", "sense",
	"sepia", "serif", "serum", "serve", "setup", "seven", "sever", "sewer", "shack", "shade",
	"shady", "shaft", "shake", "shaky", "shale", "shall", "shalt", "shame", "shank", "shape",
	"shard", "share", "shark", "sharp", "shave", "shawl", "shear", "sheen", "sheep", "sheer",
	"sheet", "sheik", "shelf", "shell", "shied", "shift", "shine", "shiny", "shire", "shirk",
	"shirt", "shoal", "shock", "shone", "shook", "shoot", "shore", "shorn", "short", "shout",
	"shove", "shown", "showy", "shrew", "shrub", "shrug", "shuck", "shunt", "shush", "shyly",
	"siege", "sieve", "sight", "sigma", "silky", "silly", "since", "sinew", "singe", "siren",
	"sissy", "sixth", "sixty", "skate", "skier", "skiff", "skill", "skimp", "skirt", "skulk",
	"skull", "skunk", "slack", "slain", "slang", "slant", "slash", "slate", "slave", "sleek",
	"sleep", "sleet", "slept", "slice", "slick", "slide", "slime", "slimy", "sling", "slink",
	"sloop", "slope", "slosh", "sloth", "slump", "slung", "slunk", "slurp", "slush", "slyly",
	"smack", "small", "smart", "smash", "smear", "smell", "smelt", "smile", "smirk", "smite",
	"smith", "smock", "smoke", "smoky", "smote", "snack", "snail", "snake", "snaky", "snare",
	"snarl", "sneak", "sneer", "snide", "sniff", "snipe", "snoop", "snore", "snort", "snout",
	"snowy", "snuck", "snuff", "soapy", "sober", "soggy", "solar", "solid", "solve", "sonar",
	"sonic", "sooth", "sooty", "sorry", "sound", "south", "sower", "space", "spade", "spank",
	"spare", "spark", "spasm", "spawn", "speak", "spear", "speck", "speed", "spell", "spelt",
	"spend", "spent", "sperm", "spice", "spicy", "spied", "spiel", "spike", "spiky", "spill",
	"spilt", "spine", "spiny", "spire", "spite", "splat", "split", "spoil", "spoke", "spoof",
	"spook", "spool", "spoon", "spore", "sport", "spout", "spray", "spree", "sprig", "spunk",
	"spurn", "spurt", "squad", "squat", "squib", "stack", "staff", "stage", "staid", "stain",
	"stair", "stake", "stale", "stalk", "stall", "stamp", "stand", "stank", "stare", "stark",
	"start", "stash", "state", "stave", "stead", "steak", "steal", "steam", "steed", "steel",
	"steep", "steer", "stein", "stern", "stick", "stiff", "still", "stilt", "sting", "stink",
	"stint", "stock", "stoic", "stoke", "stole", "stomp", "stone", "stony", "stood", "stool",
	"stoop", "store", "stork", "storm", "story", "stout", "stove", "strap", "straw", "stray",
	"strip", "strut", "stuck", "study", "stuff", "stump", "stung", "stunk", "stunt", "style",
	"suave", "sugar", "suing", "suite", "sulky", "sully", "sumac", "sunny", "super", "surer",
	"surge", "surly", "sushi", "swami", "swamp", "swarm", "swash", "swath", "swear", "sweat",
	"sweep", "sweet", "swell", "swept", "swift", "swill", "swine", "swing", "swirl", "swish",
	"swoon", "swoop", "sword", "swore", "sworn", "swung", "synod", "syrup", "tabby", "table",
	"taboo", "tacit", "tacky", "taffy", "taint", "taken", "taker", "tally", "talon", "tamer",
	"tango", "tangy", "taper", "tapir", "tardy", "tarot", "taste", "tasty", "tatty", "taunt",
	"tawny", "teach", "teary", "tease", "teddy", "teeth", "tempo", "tenet", "tenor", "tense",
	"tenth", "tepee", "tepid", "terra", "terse", "testy", "thank", "theft", "their", "theme",
	"there", "these", "theta", "thick", "thief", "thigh", "thing", "think", "third", "thong",
	"thorn", "those", "three", "threw", "throb", "throw", "thrum", "thumb", "thump", "thyme",
	"tiara", "tibia", "tidal", "tiger", "tight", "tilde", "timer", "timid", "tipsy", "titan",
	"tithe", "title", "toast", "today", "toddy", "token", "tonal", "tonga", "tonic", "tooth",
	"topaz", "topic", "torch", "torso", "torus", "total", "totem", "touch", "tough", "towel",
	"tower", "toxic", "toxin", "trace", "track", "tract", "trade", "trail", "train", "trait",
	"tramp", "trash", "trawl", "tread", "treat", "trend", "triad", "trial", "tribe", "trice",
	"trick", "tried", "tripe", "trite", "troll", "troop", "trope", "trout", "trove", "truce",
	"truck", "truer", "truly", "trump", "trunk", "truss", "trust", "truth", "tryst", "tubal",
	"tuber", "tulip", "tulle", "tumor", "tunic", "turbo", "tutor", "twang", "tweak", "tweed",
	"tweet", "twice", "twine", "twirl", "twist", "twixt", "tying", "udder", "ulcer", "ultra",
	"umbra", "uncle", "uncut", "under", "undid", "undue", "unfed", "unfit", "unify", "union",
	"unite", "unity", "unlit", "unmet", "unset", "untie", "until", "unwed", "unzip", "upper",
	"upset", "urban", "urine", "usage", "usher", "using", "usual", "usurp", "utile", "utter",
	"vague", "valet", "valid", "valor", "value", "valve", "vapid", "vapor", "vault", "vaunt",
	"vegan", "venom", "venue", "verge", "verse", "verso", "verve", "vicar", "video", "vigil",
	"vigor", "villa", "vinyl", "viola", "viper", "viral", "virus", "visit", "visor", "vista",
	"vital", "vivid", "vixen", "vocal", "vodka", "vogue", "voice", "voila", "vomit", "voter",
	"vouch", "vowel", "vying", "wacky", "wafer", "wager", "wagon", "waist", "waive", "waltz",
	"warty", "waste", "watch", "water", "waver", "waxen", "weary", "weave", "wedge", "weedy",
	"weigh", "weird", "welch", "welsh", "wench", "whack", "whale", "wharf", "wheat", "wheel",
	"whelp", "where", "which", "whiff", "while", "whine", "whiny", "whirl", "whisk", "white",
	"whole", "whoop", "whose", "widen", "wider", "widow", "width", "wield", "wight", "willy",
	"wimpy", "wince", "winch", "windy", "wiser", "wispy", "witch", "witty", "woken", "woman",
	"women", "woody", "wooer", "wooly", "woozy", "wordy", "world", "worry", "worse", "worst",
	"worth", "would", "wound", "woven", "wrack", "wrath", "wreak", "wreck", "wrest", "wring",
	"wrist", "write", "wrong", "wrote", "wrung", "wryly", "yacht", "yearn", "yeast", "yield",
	"young", "youth", "zebra", "zesty", "zonal"
]

var show_end_screen = false # variable to keep track of showing end screen

func getRandomWord():
	return wordList.pick_random() # picks random word

func _ready() -> void:
	WORD = getRandomWord() # picks random word

	$alert.text = "[center]The word you entered is not a word or not 5 letters![/center]" # this is when the user enters a word that is not part of the word list above
	for letter in WORD: # letter occurances in the answer
		if letter in letterOccurences:
			letterOccurences[letter] += 1
		else:
			letterOccurences[letter] = 1
	print(WORD)
	$giveUp.visible = true
	$backgroundMusic.playing = true # start the background music
	
func update_end_screen_visibility():
	if show_end_screen: # show all the the following when the game ends (win)
		get_node("winorlose/board").visible = true
		get_node("winorlose/textBox").visible = true
		get_node("winorlose/instruct").visible = true
		get_node("winorlose/playAgain").visible = true
		get_node("winorlose/dontPlayAgain").visible = true
	else: # show all the the following when the game ends (lose)
		get_node("winorlose/board").visible = false
		get_node("winorlose/textBox").visible = false
		get_node("winorlose/instruct").visible = false
		get_node("winorlose/playAgain").visible = false
		get_node("winorlose/dontPlayAgain").visible = false

var i = 0
func _input(event: InputEvent): # the following code takes in input from keyboard using unicode
	var key_event := event as InputEventKey
	if key_event and key_event.pressed :
		
		if i <= 4:
			# converts capital letters to lowercase
			if key_event.unicode >= 65 and key_event.unicode <= 90:
				key_event.unicode += 32
			if key_event.unicode >= 97 and key_event.unicode <= 122: 
				var letter := char(key_event.unicode)
				if not line[i].TYPED:
					line[i].TEXT = letter
					i += 1
		if key_event.keycode == KEY_BACKSPACE: # if the user wants to erase letters
			if i >= 1:
				i -= 1
				line[i].TEXT = ""
		if key_event.keycode == KEY_ESCAPE:
			get_tree().change_scene_to_file("res://menu.tscn")
			
		if key_event.keycode == KEY_ENTER: # when the user is ready to make a guess
			var userWord = ""
			
			for tile in line:
				userWord += tile.TEXT
			userGuess = userWord
			if len(userGuess) == 5 && userGuess in wordList: # if the word is valid (in the list and 5 letters)
				checkWord(userGuess)
				t += 1
				i = 0
			elif len(userGuess) == 5 && userGuess not in wordList: # if the word is 5 letters but not in the list of words alert the user to find a different word
				$alert.visible = true
				var pos = $backgroundMusic.get_playback_position()
				$backgroundMusic.stop()
				$badResponse.play()
				await get_tree().create_timer(1.5).timeout 
				$badResponse.stop()
				$backgroundMusic.play(pos)
				$alert.visible = false
			elif len(userGuess) < 5: # if the word is less than 5 letters, say its wrong
				$alert.visible = true
				var pos = $backgroundMusic.get_playback_position()
				$backgroundMusic.stop()
				$badResponse.play()
				await get_tree().create_timer(1.5).timeout 
				$badResponse.stop()
				$backgroundMusic.play(pos)
				$alert.visible = false

var t = 1
var line = []
var letterBoxDict = {}

func _process(delta):
	
	$labelTextBoxes/Game.text = "[center]Game[/center]" # label the different parts of the viewing screen
	$labelTextBoxes/Options.text = "[center]Options[/center]"
	
	line = $Line1.get_children() # set line as whichever attempt we are on
	if t == 2:
		line = $Line2.get_children()
	elif t == 3:
		line = $Line3.get_children()
	elif t == 4:
		line = $Line4.get_children()
	elif t == 5:
		line = $Line5.get_children()
	elif t == 6:
		line = $Line6.get_children()

	var alphabets = "abcdefghijklmnopqrstuvwxyz" # set up the letterBoxes on the right side
	letterBoxDict = {"a": $Line7.get_children()[0], "b": $Line7.get_children()[1], "c": $Line7.get_children()[2], "d": $Line7.get_children()[3], "e": $Line7.get_children()[4], "f": $Line8.get_children()[0], "g": $Line8.get_children()[1], "h": $Line8.get_children()[2], "i": $Line8.get_children()[3], "j": $Line8.get_children()[4], "k": $Line9.get_children()[0], "l": $Line9.get_children()[1], "m": $Line9.get_children()[2], "n": $Line9.get_children()[3], "o": $Line9.get_children()[4], "p": $Line10.get_children()[0], "q": $Line10.get_children()[1], "r": $Line10.get_children()[2], "s": $Line10.get_children()[3], "t": $Line10.get_children()[4], "u": $Line11.get_children()[0], "v": $Line11.get_children()[1], "w": $Line11.get_children()[2], "x": $Line11.get_children()[3],"y": $Line11.get_children()[4], "z": $Line12.get_children()[0]}
	for letter in alphabets:
		letterBoxDict[letter].TEXT = letter
	
var green = Color(0, 128, 0, 1) # set up colors
var yellow = Color(1, 0.8, 0, 1)
var black = Color(0, 0, 0, 1)
var grey = Color(169, 169, 169, 1)

func checkWord(userGuess): # checking if a word's letters are in the right spots or not
	
	var userGuessLetterOccurences = {} # the guess letter occurances
	
	# count number of letters in user guess
	for letter in userGuess:
		if letter in userGuessLetterOccurences:
			userGuessLetterOccurences[letter] += 1
		else:
			userGuessLetterOccurences[letter] = 1
	
	# initial highlighting and setting the colors whether or not the letters are in the right spot
	var index = 0
	for tile in line:
		var tileBackground = tile.get_children()[0]
		var tileText = tile.TEXT.to_lower()
		
		if tileText == WORD[index]:
			tileBackground.set_color(green)
		elif tileText in WORD:
			tileBackground.set_color(yellow)
		else:
			tileBackground.set_color(black)
			#
		index += 1
			
	# change duplicates to gray if not in word
	for tile in line:
		var tileBackground = tile.get_children()[0]
		var tileText = tile.TEXT.to_lower()
		
		if tileBackground.get_color() == yellow:
			if userGuessLetterOccurences[tileText] > letterOccurences[tileText]:
				tileBackground.set_color(black)
				userGuessLetterOccurences[tileText] -= 1

		# changes the letterBoxes on the right side to show what options we have for the remaining attempts 
		# green > yellow > black
		if tileBackground.get_color() == green && letterBoxDict[tileText].get_children()[0].get_color() != green:
			letterBoxDict[tileText].get_children()[0].set_color(green)
		elif tileBackground.get_color() == yellow && letterBoxDict[tileText].get_children()[0].get_color() != green:
			letterBoxDict[tileText].get_children()[0].set_color(yellow)
		elif tileBackground.get_color() == black && letterBoxDict[tileText].get_children()[0].get_color() != grey:
			letterBoxDict[tileText].get_children()[0].set_color(black)
	if WORD == userGuess: # if the user wins, show end screen and play win music
		show_end_screen = true
		update_end_screen_visibility()
		$winorlose/textBox.text = "[center]YOU WIN!!\nYOU GUESSED THE WORD IN " + str(t) + " TRIES!![/center]"
		$backgroundMusic.playing = false
		$winorlose/winMusic.playing = true
	elif WORD != userGuess && t == 6:# if the user loses, show end screen and play lose music
		show_end_screen = true
		update_end_screen_visibility()
		$winorlose/textBox.text = "[center]YOU LOSE!!\nYOU WERE UNABLE TO GUESS THE WORD: " + WORD + " IN 6 TRIES!![/center]"
		$backgroundMusic.playing = false # stop playing background music
		$winorlose/loseMusic.playing = true
		
func _on_play_again_button_up() -> void:
	get_tree().reload_current_scene() # if the user wants to play again, reload the scene

func _on_dont_play_again_button_down() -> void:
	get_tree().quit() # ifthe user wants to end the game, end it

func _on_instruct_button_down() -> void: # if the user wants to re-read the instructions, let them
	get_tree().change_scene_to_file("res://instructions.tscn")

func _on_background_music_finished() -> void: # if the background music is finished, loop it again 
	$backgroundMusic.playing = true

func _on_give_up_button_up() -> void:
	show_end_screen = true
	update_end_screen_visibility()
	$winorlose/textBox.text = "[center]YOU LOSE!!\nYOU WERE UNABLE TO GUESS THE WORD: " + WORD + " IN 6 TRIES!![/center]"
	$backgroundMusic.playing = false # stop playing background music
	$winorlose/loseMusic.playing = true
	$giveUp.visible = false
