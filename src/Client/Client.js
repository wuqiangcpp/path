window.onload = function() {
    var server="ws://222.29.69.203:5000";
    var user="wuqiang";
    var socket = new WebSocket(server);
    socket.onopen=function(event)
    {
        socket.send(user);        
        document.getElementById('status').innerHTML = 'Connected';        
    };

    var msg = {
        kind: "position",
        position: (1.2,1.2)
    };

//    var msg="hello";
//    socket.send(msg);
//    socket.send(JSON.stringify(msg));

    socket.onmessage=function (event)
    {
        var message = event.data;        
        document.getElementById("messages").innerHTML += '<li><span>Received:</span>' + message + '</li>';
//        console.log(message);
    };
}
