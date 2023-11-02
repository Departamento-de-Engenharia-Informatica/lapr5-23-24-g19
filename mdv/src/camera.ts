import * as THREE from "three";

/*
 * parameters = {
 *  view: String,
 *  backgroundColor: Color,
 *  frameColor: Color,
 *  initialViewport: Vector4,
 *  viewportSizeMin: Float,
 *  dragOrResizeThreshold: Float,
 *  snapThreshold: Float,
 *  snapPositions: [Float],
 *  initialTarget: Vector3,
 *  initialOrientation: Orientation,
 *  orientationMin: Orientation,
 *  orientationMax: Orientation,
 *  orientationStep: Orientation,
 *  initialDistance: Float,
 *  distanceMin: Float,
 *  distanceMax: Float,
 *  distanceStep: Float,
 *  initialZoom: Float,
 *  zoomMin: Float,
 *  zoomMax: Float,
 *  zoomStep: Float,
 *  initialFov: Float,
 *  near: Float,
 *  far: Float,
 *  initialFogDensity: Float // Doesn't apply to mini-map camera
 * }
 */

export default class Camera {
    constructor(parameters) {
        for (const [key, value] of Object.entries(parameters)) {
            this[key] = value;
        }

        // Compute half of the size of the target plane as a function of the camera's distance to the target and the field-of-view
        this.initialHalfSize = Math.tan(THREE.MathUtils.degToRad(this.initialFov / 2.0)) * this.initialDistance;

        // The player radius is needed to compute the position of the first-person view camera
        this.playerRadius = 0.0;

        // The player orientation is needed to compute the orientation of the first-person view, third-person view and top view cameras
        this.playerOrientation = new THREE.Quaternion().identity();

        // Create two cameras (perspective and orthographic projection)
        this.perspective = new THREE.PerspectiveCamera();
        this.orthographic = new THREE.OrthographicCamera();

        this.fogDensity = this.initialFogDensity;

        this.initialize();
    }

    percentageToPixels(viewport, windowWidth, windowHeight) { // Convert viewport position and size from % to pixels
        let width = viewport.width;
        let height = viewport.height;
        if (this.view != "mini-map") {
            width *= windowWidth;
            height *= windowHeight;
        }
        else {
            const windowSizeMin = Math.min(windowWidth, windowHeight);
            width *= windowSizeMin;
            height *= windowSizeMin;
        }
        width = Math.round(width);
        height = Math.round(height);
        const x = Math.round(viewport.x * (windowWidth - width));
        const y = Math.round(viewport.y * (windowHeight - height));
        return new THREE.Vector4(x, y, width, height);
    }

    pixelsToPercentage(viewport, windowWidth, windowHeight) { // Convert viewport position and size from pixels to %
        const deltaWidth = windowWidth - viewport.width;
        const x = deltaWidth == 0 ? 0 : viewport.x / deltaWidth;
        const deltaHeight = windowHeight - viewport.height;
        const y = deltaHeight == 0 ? 0 : viewport.y / deltaHeight;
        let width = viewport.width;
        let height = viewport.height;
        if (this.view != "mini-map") {
            width /= windowWidth;
            height /= windowHeight;
        }
        else {
            const windowSizeMin = Math.min(windowWidth, windowHeight);
            width /= windowSizeMin;
            height /= windowSizeMin;
        }
        return new THREE.Vector4(x, y, width, height);
    }

    /*
     *        Y
     *        |
     *        O -- X
     *       /
     *      Z
     */

    setViewingParameters() { // Set the camera's position, orientation and target (positive Y-semiaxis up)
        const playerOrientation = new THREE.Euler().setFromQuaternion(this.playerOrientation, "YXZ"); // Order: yaw, pitch and roll
        playerOrientation.x = -playerOrientation.x + THREE.MathUtils.degToRad(this.orientation.v);
        playerOrientation.y += THREE.MathUtils.degToRad(this.orientation.h - 180.0);
        playerOrientation.z = -playerOrientation.z;
        this.perspective.setRotationFromEuler(playerOrientation);
        this.orthographic.setRotationFromEuler(playerOrientation);

        this.perspective.position.set(this.target.x, this.target.y, this.target.z);
        this.orthographic.position.set(this.target.x, this.target.y, this.target.z);
        if (this.view == "first-person") {
            this.perspective.translateZ(-0.9 * this.playerRadius);
            this.orthographic.translateZ(-0.9 * this.playerRadius);
        }
        else {
            this.perspective.translateZ(this.distance);
            this.orthographic.translateZ(this.distance);
        }
    }

    setProjectionParameters() {
        // Compute the current aspect ratio
        this.aspectRatio = this.viewport.width / this.viewport.height;
        // Adjust the camera's field-of-view if needed; Set the left, right, top and bottom clipping planes
        let fov, left, right, top, bottom;
        if (this.aspectRatio < 1.0) {
            fov = 2.0 * THREE.MathUtils.radToDeg(Math.atan(Math.tan(THREE.MathUtils.degToRad(this.initialFov / 2.0)) / this.aspectRatio));
            right = this.initialHalfSize;
            left = -right;
            top = right / this.aspectRatio;
            bottom = -top;
        }
        else {
            fov = this.initialFov;
            top = this.initialHalfSize;
            bottom = -top;
            right = top * this.aspectRatio;
            left = -right;
        }

        // Perspective projection camera: the zoom effect is achieved by changing the value of the field-of-view; the PerspectiveCamera.zoom property does just that
        this.perspective.fov = fov;
        this.perspective.aspect = this.aspectRatio;
        this.perspective.near = this.near;
        this.perspective.far = this.far;
        this.perspective.zoom = this.zoom;
        this.perspective.updateProjectionMatrix();

        // Orthographic projection camera: the zoom effect is achieved by changing the values of the left, right, top and bottom clipping planes; the OrthographicCamera.zoom property does just that
        this.orthographic.left = left;
        this.orthographic.right = right;
        this.orthographic.top = top;
        this.orthographic.bottom = bottom;
        this.orthographic.near = this.near;
        this.orthographic.far = this.far;
        this.orthographic.zoom = this.zoom;
        this.orthographic.updateProjectionMatrix();
    }

    setActiveProjection(projection) {
        this.projection = projection;
        if (this.projection != "orthographic") {
            this.activeProjection = this.perspective;
        }
        else {
            this.activeProjection = this.orthographic;
        }
    }

    initialize() {
        this.currentViewport = this.initialViewport.clone(); // Both this.initialViewport and this.currentViewport are expressed in %
        this.target = this.initialTarget.clone();
        this.orientation = this.initialOrientation.clone();
        this.distance = this.initialDistance;
        this.zoom = this.initialZoom;

        // Set the viewport and the projection parameters (perspective: field-of-view, aspect ratio, near and far clipping planes; orthographic: left, right, top, bottom, near and far clipping planes)
        this.setViewport();

        // Set the viewing parameters (position, orientation and target)
        this.setViewingParameters();

        // Set the default camera projection: mini-map: orthographic; remaining views: perspective
        if (this.view != "mini-map") {
            this.setActiveProjection("perspective");
        }
        else {
            this.setActiveProjection("orthographic");
        }
    }

    roundViewport(viewport) {
        return new THREE.Vector4(Math.round(viewport.x), Math.round(viewport.y), Math.round(viewport.width), Math.round(viewport.height));
    }

    snapPosition(data) {
        const snapPositions = this.snapPositions.map(snapPosition => Math.round(snapPosition * data.size)); // Convert snap positions from % to pixels
        const deltaPositions = snapPositions.map(snapPosition => Math.abs(data.currentPosition - snapPosition));
        data.minDelta = Math.min(...deltaPositions);
        data.newPosition = data.minDelta < this.snapThreshold * data.size ? snapPositions[deltaPositions.indexOf(data.minDelta)] : data.currentPosition;
    }

    snapViewport(frame) { // drag: "none"; resize: string identifying the pointed frame
        let west, east;
        if (frame == "none" || frame.includes("west")) {
            west = { size: window.innerWidth, currentPosition: this.viewport.x };
            this.snapPosition(west);
        }
        if (frame == "none" || frame.includes("east")) {
            east = { size: window.innerWidth, currentPosition: (this.viewport.x + this.viewport.width) };
            this.snapPosition(east);
        }
        if (frame == "none") {
            if (west.minDelta < east.minDelta) {
                this.viewport.x = west.newPosition;
            }
            else {
                this.viewport.x = east.newPosition - this.viewport.width;
            }
        }
        else if (frame.includes("west")) {
            this.viewport.x = west.newPosition;
        }
        else if (frame.includes("east")) {
            this.viewport.width = east.newPosition - this.viewport.x;
        }

        let south, north;
        if (frame == "none" || frame.includes("south")) {
            south = { size: window.innerHeight, currentPosition: this.viewport.y };
            this.snapPosition(south);
        }
        if (frame == "none" || frame.includes("north")) {
            north = { size: window.innerHeight, currentPosition: (this.viewport.y + this.viewport.height) };
            this.snapPosition(north);
        }
        if (frame == "none") {
            if (south.minDelta < north.minDelta) {
                this.viewport.y = south.newPosition;
            }
            else {
                this.viewport.y = north.newPosition - this.viewport.height;
            }
        }
        else if (frame.includes("south")) {
            this.viewport.y = south.newPosition;
        }
        else if (frame.includes("north")) {
            this.viewport.height = north.newPosition - this.viewport.y;
        }
    }

    dragViewport(mouse) {
        const increment = mouse.currentPosition.clone().sub(mouse.initialPosition);
        this.viewport.x = THREE.MathUtils.clamp(this.previousViewport.x + increment.x, 0, window.innerWidth - this.viewport.width);
        this.viewport.y = THREE.MathUtils.clamp(this.previousViewport.y + increment.y, 0, window.innerHeight - this.viewport.height);
        this.snapViewport("none");
        this.viewport = this.roundViewport(this.viewport);
        this.currentViewport = this.pixelsToPercentage(this.viewport, window.innerWidth, window.innerHeight);
    }

    resizeViewport(frame, mouse) {
        const increment = mouse.currentPosition.clone().sub(mouse.initialPosition);
        switch (frame) {
            case "southwest":
                this.viewport.x = THREE.MathUtils.clamp(this.previousViewport.x + increment.x, 0, this.previousViewport.x + this.previousViewport.width - this.viewportWidthMin);
                this.viewport.y = THREE.MathUtils.clamp(this.previousViewport.y + increment.y, 0, this.previousViewport.y + this.previousViewport.height - this.viewportHeightMin);
                this.snapViewport(frame);
                this.viewport.width = this.previousViewport.width - (this.viewport.x - this.previousViewport.x);
                this.viewport.height = this.previousViewport.height - (this.viewport.y - this.previousViewport.y);
                break;
            case "northwest":
                this.viewport.x = THREE.MathUtils.clamp(this.previousViewport.x + increment.x, 0, this.previousViewport.x + this.previousViewport.width - this.viewportWidthMin);
                this.viewport.height = THREE.MathUtils.clamp(this.previousViewport.height + increment.y, this.viewportHeightMin, window.innerHeight - this.viewport.y);
                this.snapViewport(frame);
                this.viewport.width = this.previousViewport.width - (this.viewport.x - this.previousViewport.x);
                break;
            case "west":
                this.viewport.x = THREE.MathUtils.clamp(this.previousViewport.x + increment.x, 0, this.previousViewport.x + this.previousViewport.width - this.viewportWidthMin);
                this.snapViewport(frame);
                this.viewport.width = this.previousViewport.width - (this.viewport.x - this.previousViewport.x);
                break;
            case "southeast":
                this.viewport.y = THREE.MathUtils.clamp(this.previousViewport.y + increment.y, 0, this.previousViewport.y + this.previousViewport.height - this.viewportHeightMin);
                this.viewport.width = THREE.MathUtils.clamp(this.previousViewport.width + increment.x, this.viewportWidthMin, window.innerWidth - this.viewport.x);
                this.snapViewport(frame);
                this.viewport.height = this.previousViewport.height - (this.viewport.y - this.previousViewport.y);
                break;
            case "northeast":
                this.viewport.width = THREE.MathUtils.clamp(this.previousViewport.width + increment.x, this.viewportWidthMin, window.innerWidth - this.viewport.x);
                this.viewport.height = THREE.MathUtils.clamp(this.previousViewport.height + increment.y, this.viewportHeightMin, window.innerHeight - this.viewport.y);
                this.snapViewport(frame);
                break;
            case "east":
                this.viewport.width = THREE.MathUtils.clamp(this.previousViewport.width + increment.x, this.viewportWidthMin, window.innerWidth - this.viewport.x);
                this.snapViewport(frame);
                break;
            case "south":
                this.viewport.y = THREE.MathUtils.clamp(this.previousViewport.y + increment.y, 0, this.previousViewport.y + this.previousViewport.height - this.viewportHeightMin);
                this.snapViewport(frame);
                this.viewport.height = this.previousViewport.height - (this.viewport.y - this.previousViewport.y);
                break;
            case "north":
                this.viewport.height = THREE.MathUtils.clamp(this.previousViewport.height + increment.y, this.viewportHeightMin, window.innerHeight - this.viewport.y);
                this.snapViewport(frame);
                break;
        }
        this.viewport = this.roundViewport(this.viewport);
        this.currentViewport = this.pixelsToPercentage(this.viewport, window.innerWidth, window.innerHeight);
        this.setProjectionParameters();
    }

    adjustViewport() { // Secondary (mini-map) viewport only; make sure that its width and height are the same
        const delta = (this.viewport.height - this.viewport.width) / 2.0;
        if (delta < 0.0) {
            this.viewport.x -= delta;
            this.viewport.width = this.viewport.height;
        }
        else if (delta > 0.0) {
            this.viewport.y += delta;
            this.viewport.height = this.viewport.width;
        }
        this.viewport = this.roundViewport(this.viewport);
        this.currentViewport = this.pixelsToPercentage(this.viewport, window.innerWidth, window.innerHeight);
        this.setProjectionParameters();
    }

    setViewport(viewport) { // Expressed in fraction of window width and window height
        if (viewport !== undefined) {
            this.currentViewport = viewport;
        }
        this.viewport = this.percentageToPixels(this.currentViewport, window.innerWidth, window.innerHeight);
        this.viewportWidthMin = Math.round(this.viewportSizeMin * window.innerWidth);
        this.viewportHeightMin = Math.round(this.viewportSizeMin * window.innerHeight);
        if (this.view == "mini-map") {
            this.viewportHeightMin = this.viewportWidthMin = Math.max(this.viewportWidthMin, this.viewportHeightMin);
        }
        this.setProjectionParameters();
    }

    setTarget(target) {
        this.target.copy(target);
        this.setViewingParameters();
    }

    updateTarget(targetIncrement) {
        this.setTarget(this.target.add(targetIncrement));
    }

    setOrientation(orientation) {
        this.orientation.copy(orientation).clamp(this.orientationMin, this.orientationMax);
        this.setViewingParameters();
    }

    updateOrientation(orientationIncrement) {
        this.setOrientation(this.orientation.add(orientationIncrement));
    }

    setDistance(distance) {
        this.distance = THREE.MathUtils.clamp(distance, this.distanceMin, this.distanceMax);
        this.setViewingParameters();
    }

    updateDistance(distanceIncrement) {
        this.setDistance(this.distance + distanceIncrement);
    }

    setZoom(zoom) {
        this.zoom = THREE.MathUtils.clamp(zoom, this.zoomMin, this.zoomMax);
        this.perspective.zoom = this.zoom;
        this.perspective.updateProjectionMatrix();
        this.orthographic.zoom = this.zoom;
        this.orthographic.updateProjectionMatrix();
    }

    updateZoom(zoomIncrement) {
        this.setZoom(this.zoom + zoomIncrement);
    }
}