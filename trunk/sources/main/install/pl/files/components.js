 $(document).ready(function(){
    // Loop over each component.
    $( "#componentbox div.component" ).each(
     
    // For each component, run this code. The "intIndex" is the
    // loop iteration index on the current element.
    function( intIndex ){
    // Bind the onclick event to simply alert the
    // iteration index value.
    $( this ).bind (
        "click",
        function(){
            location.href = '/c/' + $( this ).attr('cname')
        }
    );
     
    }
     
    );
 });