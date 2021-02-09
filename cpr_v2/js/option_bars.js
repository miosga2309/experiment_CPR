$(document).ready(function() {
	$('.option-bar').click(function(e) {
		var $this = $(this);
		var $parent = $this.parents('.option-bars');

		// Set Value
		$parent.find('input').val($this.data('value'));

		// Highlight Active
		$parent.find('.option-bar.active').removeClass('active');
		$this.addClass('active');

		// Prevent redirect
		e.preventDefault();
	});
});
