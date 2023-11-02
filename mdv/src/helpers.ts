import * as THREE from "three";

export class CylinderHelper extends THREE.LineSegments {
    constructor(color) {
        super();

        this.geometry = new THREE.EdgesGeometry(new THREE.CylinderGeometry(1.0, 1.0, 2.0, 16, 1, true));
        this.material = new THREE.LineBasicMaterial({ color: color });
    }
}

export class BoxHelper extends THREE.LineSegments {
    constructor(color) {
        super();

        this.geometry = new THREE.EdgesGeometry(new THREE.BoxGeometry(2.0, 2.0, 2.0));
        this.material = new THREE.LineBasicMaterial({ color: color });
    }
}