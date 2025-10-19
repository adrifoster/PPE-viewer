$(document).ready(function() {
  // Find all selectize inputs that allow multiple selection.
  $('.selectize-input.multi').each(function() {
    var $selectizeElement = $(this).closest('.form-group').find('select');
    var selectizeInstance = $selectizeElement[0].selectize;

    // Attach a click event handler to the items within the selectize input.
    selectizeInstance.$control.on('click', '.item', function(e) {
      var value = $(this).attr('data-value');
      var existingSelection = selectizeInstance.getValue();
      
      // If the clicked item is already selected, deselect it.
      if (existingSelection.includes(value)) {
        var newSelection = existingSelection.filter(item => item !== value);
        selectizeInstance.setValue(newSelection);
        e.stopPropagation(); // Prevent dropdown from opening
      }
    });
  });
});