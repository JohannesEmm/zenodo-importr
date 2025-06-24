# Zenodo BibTeX Importer (zenodo-importr)

A Shiny application that streamlines the process of uploading academic publications to Zenodo. This tool automatically processes BibTeX files and uploads publications with attached PDF files to your Zenodo repository.

## 🚀 Features

- **Simple BibTeX Processing**: Upload BibTeX files and automatically extract publication metadata
- **PDF Attachment**: Automatically attach PDF files to your publications
- **Zenodo Integration**: Direct upload to Zenodo via API with proper metadata formatting
- **EC Grant Support**: Add European Commission grant numbers to your publications
- **Author ORCID Integration**: Optional Excel-based author ORCID configuration
- **Access Control**: Support for both open access and closed access publications
- **Auto-Publishing**: Option to automatically publish uploads or save as drafts

## 📋 Prerequisites

- R (version 4.0 or higher)
- Zenodo account with API access token
- Required R packages (see Installation)

## 🛠️ Installation

1. **Clone the repository:**
   ```bash
   git clone https://github.com/yourusername/zenodo-bibtex-importer.git
   cd zenodo-bibtex-importer
   ```

2. **Set up Zenodo API Token:**
   - Go to [Zenodo Token Settings](https://zenodo.org/account/settings/applications/tokens/new/)
   - Create a new token with `deposit:write` scope
   - Copy the token for use in the application

## 🎯 How to Use

### Step 1: Start the Application
```r
# Run the Shiny app
source("app.R")  # or whatever you name your main file
shinyApp(ui = ui, server = server)
```

### Step 2: Upload Your Files
1. **Upload BibTeX File**: Choose a `.bib` file containing your publication(s)
   - Only the first entry will be processed and uploaded
   - The app will display publication details for review

2. **Upload PDF File**: Choose a PDF file to attach to your publication
   - If multiple PDFs are selected, only the first one will be used
   - The PDF will be automatically attached to the publication

### Step 3: Configure Upload Settings
- **Access Token**: Enter your Zenodo API token
- **Access Type**: Choose between "Open Access" or "Closed Access"
- **Auto-publish**: Enable to automatically publish after upload (otherwise saves as draft)
- **EC Grants**: Optionally add European Commission grant numbers (one per line)

### Step 4: Upload to Zenodo
- Click "Upload to Zenodo" to start the process
- Monitor the console output for detailed progress information
- View your uploads on Zenodo using the provided link

## 📁 File Structure

```
zenodo-bibtex-importer/
├── app.R                 # Main Shiny application
├── README.md            # This file
└── examples/
    ├── sample.bib       # Example BibTeX file
    └── authors.xlsx     # Example ORCID configuration
```

## ⚙️ Configuration Options

### Author ORCID Configuration (Optional)
Create an Excel file with the following columns:
- `Name`: Author name as it appears in your BibTeX
- `ORCID`: Corresponding ORCID identifier

Upload this file in the "Settings" tab to automatically add ORCID IDs to your publications.

### Grant Numbers
Enter European Commission grant numbers in the text area, one per line. Example:
```
101022622
956811
```

## 🔧 Technical Details

### Supported Metadata Fields
The tool extracts and uploads the following metadata from BibTeX:
- Title
- Authors (with optional ORCID integration)
- Abstract/Description
- Publication date
- Journal information (title, volume, issue, pages)
- DOI
- Publication type
- Language (defaults to English)

### API Integration
- Uses Zenodo REST API v1
- Production Zenodo only (no sandbox support)
- Requires `deposit:write` permission scope
- Supports file uploads up to Zenodo's limits

### File Upload Process
1. Creates a new deposition on Zenodo
2. Uploads metadata in JSON format
3. Uploads PDF file to the deposition bucket
4. Optionally publishes the deposition

## 🐛 Troubleshooting

### Common Issues

**"Access forbidden" error:**
- Check that your API token has `deposit:write` scope
- Ensure you're using a production Zenodo token (not sandbox)
- Verify the token is correctly entered in the application

**PDF not uploading:**
- Ensure PDF file exists and is accessible
- Check file size doesn't exceed Zenodo limits
- Verify PDF file format is valid

**BibTeX parsing errors:**
- Ensure BibTeX file is valid and properly formatted
- Check that the first entry contains required fields (title, author)
- Try with a simplified BibTeX entry for testing

### Debug Information
The application provides detailed console output for troubleshooting:
- File upload progress
- API response codes
- Metadata processing details
- Error messages with specific failure reasons

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch (`git checkout -b feature/amazing-feature`)
3. Commit your changes (`git commit -m 'Add some amazing feature'`)
4. Push to the branch (`git push origin feature/amazing-feature`)
5. Open a Pull Request

## 🙏 Acknowledgments

- [Zenodo](https://zenodo.org/) for providing the repository platform and API
- [RefManageR](https://docs.ropensci.org/RefManageR/) for BibTeX processing
- [Shiny](https://shiny.rstudio.com/) for the web application framework

## 📞 Support

If you encounter issues or have questions:
1. Check the troubleshooting section above
2. Review the console output for error details
3. Open an issue on GitHub with detailed information about your problem

## 🔗 Related Links

- [Zenodo API Documentation](https://developers.zenodo.org/)
- [BibTeX Format Guide](https://en.wikipedia.org/wiki/BibTeX)
- [ORCID Identifier System](https://orcid.org/)
- [European Commission Grant Database](https://cordis.europa.eu/)

---

**Note**: This tool uploads to production Zenodo. Test uploads will appear in your actual Zenodo account. Use caution and consider the implications before publishing.
