function randomStr(len) {
　　len = len || 32;
　　var $chars = 'ABCDEFGHJKMNPQRSTWXYZabcdefhijkmnprstwxyz2345678';
　　var maxPos = $chars.length;
　　var pwd = '';
　　for (i = 0; i < len; i++) {
　　　　pwd += $chars.charAt(Math.floor(Math.random() * maxPos));
　　}
　　return pwd;
}

function getParameter(name)  
{
    var paramStr=location.search;
    if(paramStr.length==0) return null;
    if(paramStr.charAt(0)!='?') return null;
    paramStr=unescape(paramStr);
    paramStr=paramStr.substring(1);
    if(paramStr.length==0) return null;
    var params=paramStr.split('&');
    for(var i=0;i<params.length;i++)
    {
	var parts=params[i].split('=',2);
	if(parts[0]==name)
	{
	    if(parts.length<2||typeof(parts[1])=="undefined"||parts[1]=="undefined"||parts[1]=="null") return "";
	    return parts[1];
	}
    }
    return null;
}


window.onload = function() {

    // Get references to elements on the page.
   // var form = document.getElementById('user-form');
//    var userField = document.getElementById('user');
//    var messagesList = document.getElementById('messages');
    var socketStatus = document.getElementById('status');
    //    var closeBtn = document.getElementById('close');
    var debugShow=document.getElementById('debug');


    //var username=getParameter("user");//randomStr(6);//"wuqiang";
    var username=randomStr(6);    
    var server='ws://222.29.69.203:5000';
    //debugShow.innerHTML=JSON.stringify(username);    
    
    // Create a new WebSocket.
    var socket = new WebSocket(server);


    // Handle any errors that occur.
    socket.onerror = function(error) {
	console.log('WebSocket Error: ' + error);
    };



    // Show a connected message when the WebSocket is opened.
    socket.onopen = function(event) {
	socketStatus.innerHTML = 'Connected';
	socketStatus.className = 'open';
	socket.send(username);
    };


    // form.onsubmit = function(e) {
    // 	e.preventDefault();
    // 	var user = userField.value;
    // 	username=user;
    // 	socket.send(username);	
    // 	var pos = {
    //         kind: "position",
    // 	    user: username,
    //         position: positions[username]
    // 	};
    // 	socket.send(JSON.stringify(pos) );	
    //    }


    positions={};
    lastPositions={};
    paths=new Array();
    var prompt=false;
    function onVisibilityChanged(event) {
	var hidden = event.target.webkitHidden;
	if (hidden){
	    prompt=true;

	}else {
	    prompt=false;
	}
    }
    document.addEventListener("webkitvisibilitychange", onVisibilityChanged, false);
    
    // Handle messages sent by the server.
    socket.onmessage = function(event) {
	var message = event.data;
	var obj=JSON.parse(message);
	//debugShow.innerHTML=JSON.stringify(message);
	if(obj.msgType=="textMsg")
	{
	    if(obj.msg=="Died")
	    {
		if(!prompt)
		{
		    prompt=true;
		    var newLife=confirm("New Life");
		    if(newLife)
		    {
			var New = {
			    msgType: "textMsg",
			    msg: "New"
			};
			socket.send(JSON.stringify(New) );
			window.location.reload();
		    }
		}
	    }
	}
	else if(obj.msgType=="delete" )
	{
	    var usr=obj.msg;
//	    debugShow.innerHTML=JSON.stringify(message);	    
	    delete positions[usr];
	    delete lastPositions[usr];
	}
	else if(obj.msgType=="clientMsg")
	{
	    var msg=obj.msg
	    var I=positions[username];		
	    var pos=msg.pos;
	    var lastPos=msg.lastPos;
	    positions[msg.user]=pos;
	    lastPositions[msg.user]=lastPos;
	}
	else if(obj.msgType=="pathMsg")
	{
	    var path=obj.msg	    
	    paths.push({start:path.start,end:path.end});
	}
//	debugShow.innerHTML=JSON.stringify(lastPositions);		
//	debugShow.innerHTML+=JSON.stringify(positions);	
	
//	messagesList.innerHTML += '<li class="received"><span>Received:</span>' +
	//            message + '</li>';
    };


    // Show a disconnected message when the WebSocket is closed.
    socket.onclose = function(event) {
	socketStatus.innerHTML = 'Disconnected from WebSocket.';
	socketStatus.className = 'closed';
    };


    // Send a message when the form is submitted.
    // form.onsubmit = function(e) {
    // 	// e.preventDefault();

    // 	// Retrieve the message from the textarea.
    // 	var message = messageField.value;

    // 	//   // Send the message through the WebSocket.
    // 	socket.send(JSON.stringify(message) );



	

    // 	// Add the message to the messages list.
    // 	messagesList.innerHTML += '<li class="sent"><span>Sent:</span>' + message +
    //         '</li>';

    // 	// Clear out the message field.
    // 	messageField.value = '';

    // 	return false;
    // };


    // Close the WebSocket connection when the close button is clicked.
    // closeBtn.onclick = function(e) {
    // 	e.preventDefault();

    // 	// Close the WebSocket.
    // 	socket.close();

    // 	return false;
    // };
    var width=350;//document.documentElement.clientWidth;
    var height=550;//document.documentElement.clientHeight;
    var canvas = document.getElementById('map');
    canvas.width=width;
    canvas.height=height;
    canvas.style.backgroundColor="#6d747f";
    var moving = false;
    var mousePos = { x:0, y:0 };
    var lastPos=mousePos;
//    var turnAngle=0;
    var direction=[0,0];
    var v=1;
    canvas.addEventListener("mousedown", function (e) {
        moving = true;
	mousePos = getMousePos(canvas, e);
	var dx=mousePos.x-width/2;
	var dy=mousePos.y-height/2;
	var dm=Math.sqrt(dx*dx+dy*dy);
	var new_direction=[dx/dm,dy/dm];
	direction=new_direction;
	lastPos.x=positions[username][0];
	lastPos.y=positions[username][1];
    }, false);
    canvas.addEventListener("mouseup", function (e) {
	moving = false;
	var Stop = {
	       msgType: "textMsg",
	       msg: "Stop"	};
	socket.send(JSON.stringify(Stop));
    }, false);
    // canvas.addEventListener("mousemove", function (e) {
    // 	mousePos = getMousePos(canvas, e);
    // }, false);

    // Get the position of the mouse relative to the canvas
    function getMousePos(canvasDom, mouseEvent) {
	var rect = canvasDom.getBoundingClientRect();
	return {
	    x: mouseEvent.clientX - rect.left,
	    y: mouseEvent.clientY - rect.top
	};
    }




    canvas.addEventListener("touchstart", function (e) {
        mousePos = getTouchPos(canvas, e);
	var touch = e.touches[0];
	var mouseEvent = new MouseEvent("mousedown", {
	    clientX: touch.clientX,
	    clientY: touch.clientY
	});
	canvas.dispatchEvent(mouseEvent);
    }, false);
    canvas.addEventListener("touchend", function (e) {
	var mouseEvent = new MouseEvent("mouseup", {});
	canvas.dispatchEvent(mouseEvent);
    }, false);
    // canvas.addEventListener("touchmove", function (e) {
    // 	var touch = e.touches[0];
    // 	var mouseEvent = new MouseEvent("mousemove", {
    // 	    clientX: touch.clientX,
    // 	    clientY: touch.clientY
    // 	});
    // 	canvas.dispatchEvent(mouseEvent);
    // }, false);

    // Get the position of a touch relative to the canvas
    function getTouchPos(canvasDom, touchEvent) {
	var rect = canvasDom.getBoundingClientRect();
	return {
	    x: touchEvent.touches[0].clientX - rect.left,
	    y: touchEvent.touches[0].clientY - rect.top
	};
    }

    
    document.body.addEventListener("touchstart", function (e) {
	if (e.target == canvas) {
	    e.preventDefault();
	}
    }, false);
    document.body.addEventListener("touchend", function (e) {
	if (e.target == canvas) {
	    e.preventDefault();
	}
    }, false);
    document.body.addEventListener("touchmove", function (e) {
	if (e.target == canvas) {
	    e.preventDefault();
	}
    }, false);
    
    




    

    
    var lastDrawTime;
    // var sun = new Image();
    // var moon = new Image();
    // var earth = new Image();
    function init() {
	// sun.src = './pen.jpeg';
	// moon.src = './pen.jpeg';
	// earth.src = './pen.jpeg';
	// var x=Math.floor((Math.random() * 300) + 1)-150;
	// var y=Math.floor((Math.random() * 300) + 1)-150;
	// positions[username]=[x,y];
	lastDrawTime=performance.now();
	window.requestAnimationFrame(draw);
    }
    function draw(now) {
	var elapsed = now - lastDrawTime;
	debugShow.innerHTML=JSON.stringify(elapsed);	

	var ctx = canvas.getContext('2d');

//	ctx.globalCompositeOperation = 'destination-over';
	ctx.clearRect(0, 0, width, height); // clear canvas

	
//	ctx.fillStyle = 'rgba(0, 0, 0, 0.4)';
//	ctx.strokeStyle = 'rgba(0, 153, 255, 0.4)';
	ctx.save();
	ctx.translate(width/2, height/2);
	var lineWidth=20;
	ctx.fillStyle = "blue";		
	ctx.lineWidth = lineWidth;
	ctx.strokeStyle = "blue";	
	// Earth
	// var time = new Date();
	// ctx.rotate(((2 * Math.PI) / 60) * time.getSeconds() + ((2 * Math.PI) / 60000) * time.getMilliseconds());
	// ctx.translate(105, 0);
//	ctx.fillRect(0, -12, 50, 24); // Shadow
	//	ctx.drawImage(earth, -12, -12);


	if(moving)
	{
	    positions[username][0]+=direction[0]*v*elapsed/40;
	    positions[username][1]+=direction[1]*v*elapsed/40;
	    var pos = {
		msgType:"clientMsg",
		msg:{
    		    user: username,
		    //		birth:{s:0,ns:0},
		    //		life:0,
		    pos: positions[username]
		    //		lastPos:(0,0)
		}
    	    };
    	    socket.send(JSON.stringify(pos) );

	    // ctx.beginPath();
	    // ctx.moveTo(lastPos.x-positions[username][0], lastPos.y-positions[username][1]);
	    // ctx.lineTo(0,0);
	    // ctx.stroke();
	    
	}
	
	var I=positions[username];
	for(var key in positions)
	{
	    if(positions[key]&&lastPositions[key])
	    {
		var x=positions[key][0]-I[0];
		var y=positions[key][1]-I[1];
		ctx.fillRect(x-5,y-5,10,10);
		ctx.beginPath();
		ctx.save();
		ctx.lineWidth=0;
		ctx.arc(lastPositions[key][0]-I[0],lastPositions[key][1]-I[1],lineWidth/2,0,2*Math.PI,false);
		ctx.fill();		
		ctx.restore();
		
		ctx.beginPath();
		ctx.moveTo(lastPositions[key][0]-I[0], lastPositions[key][1]-I[1]);
		ctx.lineTo(positions[key][0]-I[0], positions[key][1]-I[1]);
		ctx.stroke();
	    }
	}

	for(var key in paths)
	{
	    if(paths[key])
	    {
		var start=paths[key]["start"];
		var end=paths[key]["end"];
		ctx.beginPath();
		ctx.save();
		ctx.lineWidth=0;
		ctx.arc(start[0]-I[0],start[1]-I[1],lineWidth/2,0,2*Math.PI,false);
		ctx.fill();		
		ctx.restore();
		
		ctx.beginPath();		
		ctx.moveTo(start[0]-I[0], start[1]-I[1]);
		ctx.lineTo(end[0]-I[0], end[1]-I[1]);
		ctx.stroke();
	    }
	}
//	debugShow.innerHTML=JSON.stringify(paths);

	// Moon
	// ctx.save();
	// ctx.rotate(((2 * Math.PI) / 6) * time.getSeconds() + ((2 * Math.PI) / 6000) * time.getMilliseconds());
	// ctx.translate(0, 28.5);
	// ctx.drawImage(moon, -3.5, -3.5);
	// ctx.restore();
	ctx.restore();
	
	// ctx.beginPath();
	// ctx.arc(150, 150, 105, 0, Math.PI * 2, false); // Earth orbit
	// ctx.stroke();
	
//	ctx.drawImage(sun, 0, 0, 300, 300);
	lastDrawTime=performance.now();;
	window.requestAnimationFrame(draw);
    }
    init();   

};
