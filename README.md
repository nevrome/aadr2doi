# aadr2doi

A small cli tool to resolve publication keys used in the [Allen Ancient DNA Resource (AADR)](https://reich.hms.harvard.edu/allen-ancient-dna-resource-aadr-downloadable-genotypes-present-day-and-ancient-dna-data) to DOIs.

### Setup

1. Install the Haskell build tool [Stack](https://docs.haskellstack.org)
2. Clone the repository
3. Execute `stack install` inside the repository to build the tool and automatically copy the executables to `~/.local/bin` (which you may want to add to your path). This will install the compiler and all dependencies into folders that won't interfere with any installation you might already have.

### Workflow

I built this package for a workflow to resolve AADR paper keys to DOIs and then DOIs to BibTeX with the [doi2bib](https://github.com/bibcure/doi2bib) CLI tool. For example:

```bash
# Resolve DOIs for three papers and store them in DOIs.txt
aadr2doi -k "OlaldeScience2019,WangNatureCommunications2019,FernandesNatureEcologyEvolution2020" --aadrVersion 50.0 -o DOIs.txt
# Resolve the DOIs to BibTeX entries
doi2bib -i DOIs.txt -o bibtex.bib
```

Make sure to set the correct `--aadrVersion`. Later versions are not reliable supersets of earlier versions.

### Interface

```
Usage: aadr2doi [--version] ((-k|--keys ARG) | (-i|--inFile ARG) | (-l|--list))
                [-s|--doiShape ARG] [--printKey] [--aadrVersion ARG]
                [-o|--outFile ARG]
  Resolve the paper keys used by the AADR to DOIs

Available options:
  -h,--help                Show this help text
  --version                Show version
  -k,--keys ARG            Paper keys to resolve. Multiple entries separated by
                           comma, so e.g.
                           "SaupeScheibCurrBio2021,RobbeetsNingNature2021"
  -i,--inFile ARG          File with paper keys to resolve. One key per line
  -l,--list                Don't resolve any keys, just return a list of all
                           available papers with their keys and DOIs
  -s,--doiShape ARG        Return DOIs as URL ("URL") or just with the id string
                           ("Short") (default: URL)
  --printKey               Print the input paper keys again as part of the
                           output
  --aadrVersion ARG        The AADR version to be queried. As of January 2023
                           one of: "54.1", "52.2", "50.0", "50.0", "44.3",
                           "42.4" (default: "54.1")
```

### Known issues

- For some papers the AADR website does not feature DOIs, so they can not be resolved.
- For some papers the .anno file has a different key than the one used on the website.
- For some papers the AADR lists the keys in a non-standard way or offers multiple keys for one paper. `aadr2doi` can not handle these special cases.

Please check the website for these special cases and resolve them manually.

