import * as THREE from "three";
import { merge } from "./merge";
import Orientation from "./orientation";

/*
 * parameters = {
 *  visible: Boolean,
 *  color: Color,
 *  intensity: Float,
 *  intensityMin: Float,
 *  intensityMax: Float,
 *  intensityStep: Float
 * }
 */

export class AmbientLight extends THREE.AmbientLight {
    constructor(parameters) {
        super();
        merge(this, parameters);
    }
}

/*
 * parameters = {
 *  visible: Boolean,
 *  color: Color,
 *  intensity: Float,
 *  intensityMin: Float,
 *  intensityMax: Float,
 *  intensityStep: Float,
 *  distance: Float,
 *  orientation: Orientation,
 *  orientationMin: Orientation,
 *  orientationMax: Orientation,
 *  orientationStep: Orientation,
 *  castShadow: Boolean,
 *  shadow: {
 *   mapSize: Vector2,
 *   camera: {
 *    left: Float,
 *    right: Float,
 *    top: Float,
 *    bottom: Float,
 *    near: Float,
 *    far: Float
 *   }
 *  }
 * }
 */

export class DirectionalLight extends THREE.DirectionalLight {
    constructor(parameters) {
        super();
        merge(this, parameters);

        // Set light position
        const position = this.orientationToPosition(this.distance, this.orientation);
        this.position.set(position.x, position.y, position.z);
    }

    orientationToPosition(distance, orientation) {
        const cosH = Math.cos(THREE.MathUtils.degToRad(orientation.h));
        const sinH = Math.sin(THREE.MathUtils.degToRad(orientation.h));
        const cosV = Math.cos(THREE.MathUtils.degToRad(orientation.v));
        const sinV = Math.sin(THREE.MathUtils.degToRad(orientation.v));
        const positionX = distance * sinH * cosV;
        const positionY = distance * sinV;
        const positionZ = distance * cosH * cosV;
        return new THREE.Vector3(positionX, positionY, positionZ);
    }
}

/*
 * parameters = {
 *  visible: Boolean,
 *  color: Color,
 *  intensity: Float,
 *  intensityMin: Float,
 *  intensityMax: Float,
 *  intensityStep: Float,
 *  distance: Float,
 *  distanceMin: Float,
 *  distanceMax: Float,
 *  distanceStep: Float,
 *  angle: Float,
 *  angleMin: Float,
 *  angleMax: Float,
 *  angleStep: Float,
 *  penumbra: Float,
 *  penumbraMin: Float,
 *  penumbraMax: Float,
 *  penumbraStep: Float,
 *  position: Vector3,
 *  positionMin: Vector3,
 *  positionMax: Vector3,
 *  positionStep: Vector3,
 *  castShadow: Boolean,
 *  shadow: {
 *   mapSize: Vector2,
 *   camera: {
 *    near: Float,
 *    far: Float
 *   },
 *   focus: Float
 *  }
 * }
 */

export class SpotLight extends THREE.SpotLight {
    constructor(parameters) {
        super();
        merge(this, parameters);

        // Convert this light's angle from degrees to radians
        this.angle = THREE.MathUtils.degToRad(this.angle);
    }
}

/*
 * parameters = {
 *  visible: Boolean,
 *  color: Color,
 *  intensity: Float,
 *  intensityMin: Float,
 *  intensityMax: Float,
 *  intensityStep: Float,
 *  distance: Float,
 *  distanceMin: Float,
 *  distanceMax: Float,
 *  distanceStep: Float,
 *  angle: Float,
 *  angleMin: Float,
 *  angleMax: Float,
 *  angleStep: Float,
 *  penumbra: Float,
 *  penumbraMin: Float,
 *  penumbraMax: Float,
 *  penumbraStep: Float,
 *  orientation: Orientation,
 *  orientationMin: Orientation,
 *  orientationMax: Orientation,
 *  orientationStep: Orientation,
 *  castShadow: Boolean,
 *  shadow: {
 *   mapSize: Vector2,
 *   camera: {
 *    near: Float,
 *    far: Float
 *   },
 *   focus: Float
 *  }
 * }
 */

export class FlashLight extends THREE.SpotLight {
    constructor(parameters) {
        super();
        merge(this, parameters);

        // Convert this light's angle from degrees to radians
        this.angle = THREE.MathUtils.degToRad(this.angle);

        // The player radius is needed to compute the position of this light
        this.playerRadius = 0.0;

        // The player orientation is needed to compute the orientation of this light
        this.playerOrientation = new THREE.Quaternion().identity();
    }

    orientationToPosition(distance, orientation) {
        const cosH = Math.cos(THREE.MathUtils.degToRad(orientation.h));
        const sinH = Math.sin(THREE.MathUtils.degToRad(orientation.h));
        const cosV = Math.cos(THREE.MathUtils.degToRad(orientation.v));
        const sinV = Math.sin(THREE.MathUtils.degToRad(orientation.v));
        const positionX = distance * sinH * cosV;
        const positionY = distance * sinV;
        const positionZ = distance * cosH * cosV;
        return new THREE.Vector3(positionX, positionY, positionZ);
    }

    // Set this light's position, orientation and target (positive Y-semiaxis up)
    setLightingParameters() {
        const playerOrientation = new THREE.Euler().setFromQuaternion(this.playerOrientation, "YXZ"); // Order: yaw, pitch and roll
        playerOrientation.x = THREE.MathUtils.radToDeg(-playerOrientation.x) + this.orientation.v;
        playerOrientation.y = THREE.MathUtils.radToDeg(playerOrientation.y) + this.orientation.h;
        playerOrientation.z = THREE.MathUtils.radToDeg(-playerOrientation.z);
        const target = this.orientationToPosition(this.distance, new Orientation(playerOrientation.y, playerOrientation.x));
        this.target.translateX(target.x);
        this.target.translateY(target.y);
        this.target.translateZ(target.z);
    }

    setTarget(target) {
        this.position.set(target.x, target.y, target.z);
        this.target.position.set(target.x, target.y, target.z);
        this.setLightingParameters();
    }
}
