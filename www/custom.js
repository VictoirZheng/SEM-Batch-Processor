// Custom JavaScript for SEM Batch Processor
// Implements drag-and-drop file upload and other interactive features

$(document).ready(function() {
  
  // Drag and drop file upload
  var dropZone = document.getElementById('drag_drop_zone');
  
  if (dropZone) {
    // Prevent default drag behaviors
    ['dragenter', 'dragover', 'dragleave', 'drop'].forEach(eventName => {
      dropZone.addEventListener(eventName, preventDefaults, false);
      document.body.addEventListener(eventName, preventDefaults, false);
    });
    
    // Highlight drop zone when item is dragged over it
    ['dragenter', 'dragover'].forEach(eventName => {
      dropZone.addEventListener(eventName, highlight, false);
    });
    
    ['dragleave', 'drop'].forEach(eventName => {
      dropZone.addEventListener(eventName, unhighlight, false);
    });
    
    // Handle dropped files
    dropZone.addEventListener('drop', handleDrop, false);
  }
  
  function preventDefaults(e) {
    e.preventDefault();
    e.stopPropagation();
  }
  
  function highlight(e) {
    dropZone.classList.add('drag-over');
  }
  
  function unhighlight(e) {
    dropZone.classList.remove('drag-over');
  }
  
  function handleDrop(e) {
    var dt = e.dataTransfer;
    var files = dt.files;
    
    if (files.length > 0) {
      // Get the file input element
      var fileInput = document.getElementById('data_file');
      
      if (fileInput) {
        // Create a new FileList and assign it to the input
        var dataTransfer = new DataTransfer();
        dataTransfer.items.add(files[0]);
        fileInput.files = dataTransfer.files;
        
        // Trigger change event
        var event = new Event('change', { bubbles: true });
        fileInput.dispatchEvent(event);
        
        // Show notification
        Shiny.setInputValue('file_dropped', files[0].name, {priority: 'event'});
      }
    }
  }
  
  // Update workflow progress based on current tab
  $(document).on('shiny:inputchanged', function(event) {
    if (event.name === 'sidebar_menu') {
      var progress = 0;
      switch(event.value) {
        case 'home': progress = 0; break;
        case 'data': progress = 20; break;
        case 'model': progress = 40; break;
        case 'batch': progress = 60; break;
        case 'results': progress = 80; break;
        case 'visualization': progress = 100; break;
      }
      
      // Update progress bar
      var progressBar = document.querySelector('#workflow_progress');
      if (progressBar) {
        progressBar.style.width = progress + '%';
        progressBar.textContent = progress + '%';
      }
    }
  });
  
  // Add tooltips
  $('[data-toggle="tooltip"]').tooltip();
  
  // Smooth scrolling
  $('a[href^="#"]').on('click', function(e) {
    e.preventDefault();
    var target = this.hash;
    var $target = $(target);
    
    $('html, body').stop().animate({
      'scrollTop': $target.offset().top
    }, 900, 'swing');
  });
  
});

// Add CSS for drag-over state
var style = document.createElement('style');
style.innerHTML = `
  .drag-over {
    border-color: #3c8dbc !important;
    background-color: #e8f4f8 !important;
  }
`;
document.head.appendChild(style);
