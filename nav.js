
// convenience function for document.getElementById().
window['$']=function(a){return document.getElementById(a)};


/////////////////////// debug flag ////////////////////////
var debug = false;


/////////////////////// adjustable parameters //////////////////
var minStep = 10;
var nSteps = 30;
var stepInterval = 10;
var blockRange = 5;                    // how far consider one page blocked
var nodeHLColor = '#C9B0A9';
var lineHLColor = '#FFFF66';
var lineBlockedColor = '#E9AB17';
var bgColor = '';
var bodyBlockedColor = '#FAF0E6';


///////////////////////// globals ////////////////////////
var eventCount = { 'left' : 0, 'right' : 0};
var moving = false;
var matchId1 = 'leftstart';
var matchId2 = 'rightstart';
var matchLineId1 = -1;
var matchLineId2 = -1;
var cTimeout;


///////////////////////// utilities ///////////////////////

// No Math.sign() in JS?
function sign(x) {
    if (x > 0) {
        return 1;
    } else if (x < 0) {
        return -1;
    } else {
        return 0;
    }
}


function log(msg) {
    if (debug) {
        console.log(msg);
    }
}



function elementPosition(id) {
    obj = $(id);
    var curleft = 0, curtop = 0;

    if (obj && obj.offsetParent) {
        curleft = obj.offsetLeft;
        curtop = obj.offsetTop;

        while (obj = obj.offsetParent) {
            curleft += obj.offsetLeft;
            curtop += obj.offsetTop;
        }
    }

    return { x: curleft, y: curtop };
}


/*
 * Scroll the window to relative position, detecting blocking positions.
 */
function scrollWithBlockCheck(container, distX, distY) {
    var oldTop = container.scrollTop;
    var oldLeft = container.scrollLeft;

    container.scrollTop += distY;      // the ONLY place for actual scrolling
    container.scrollLeft += distX;

    var actualX = container.scrollLeft - oldLeft;
    var actualY = container.scrollTop - oldTop;
    log("distY=" + distY + ", actualY=" + actualY);
    log("distX=" + distX + ", actualX=" + actualX);

    // extra leewaw here because Chrome scrolling is horribly inacurate
    if ((Math.abs(distX) > blockRange && actualX === 0)
        || Math.abs(distY) > blockRange && actualY === 0) {
        log("blocked");
        container.style.backgroundColor = bodyBlockedColor;
        return true;
    } else {
        eventCount[container.id] += 1;
        container.style.backgroundColor = bgColor;
        return false;
    }
}


function getContainer(elm) {
    while (elm && elm.tagName !== 'DIV') {
        elm = elm.parentElement || elm.parentNode;
    }
    return elm;
}


/*
 * timed animation function for scrolling the current window
 */
function matchWindow(linkId, targetId, n)
{
    moving = true;

    var link = $(linkId);
    var target = $(targetId);
    var linkContainer = getContainer(link);
    var targetContainer = getContainer(target);

    var linkPos = elementPosition(linkId).y - linkContainer.scrollTop;
    var targetPos = elementPosition(targetId).y - targetContainer.scrollTop;
    var distY = targetPos - linkPos;
    var distX = linkContainer.scrollLeft - targetContainer.scrollLeft;


    log("matching window... " + n + " distY=" + distY + " distX=" + distX);

    if (distY === 0 && distX === 0) {
        clearTimeout(cTimeout);
        moving = false;
    } else if (n <= 1) {
        scrollWithBlockCheck(targetContainer, distX, distY);
        moving = false;
    } else {
        var stepSize = Math.floor(Math.abs(distY) / n);
        actualMinStep = Math.min(minStep, Math.abs(distY));