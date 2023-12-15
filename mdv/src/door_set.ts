import * as THREE from 'three'
import { merge } from './merge'
import Door, { DoorParams } from './door'
import Maze, { DoorT } from './maze'
import ThumbRaiser from './thumb_raiser'

export type DoorSetParameters = {
    maze: Maze
}
export default class DoorSet extends THREE.Group {
    public doors: Door[] = []

    constructor(
        parameters: DoorSetParameters,
        doorParameters: DoorParams,
        scale: THREE.Vector3,
    ) {
        super()
        merge(this, parameters, doorParameters)

        parameters.maze.rooms.forEach((e) => {
            let position: THREE.Vector3 = new THREE.Vector3()
            position = parameters.maze.cellToCartesian([e.x, e.y])

            const params = {
                modelUri: doorParameters.modelUri,
                name: e.name,
                scale: new THREE.Vector3(0.2, 0.23, 0.4),
                helpersColor: new THREE.Color(0xffffff),
                defaultDirection: 0.0,
                credits: doorParameters.credits,
            }
            const door = new Door(params)

            console.log('Door', ' at x=', e.x, ' and y=', e.y)

            door.position.set(position.x, position.y, position.z)

            switch (e.orientation) {
                case 'E':
                    door.rotateY(THREE.MathUtils.degToRad(180))
                    door.position.set(position.x + 0.5, position.y, position.z)
                    break
                case 'N':
                    door.rotateY(THREE.MathUtils.degToRad(270))
                    door.position.set(position.x, position.y, position.z - 0.5)
                    break
                case 'W':
                    door.rotateY(THREE.MathUtils.degToRad(0))
                    door.position.set(position.x - 0.5, position.y, position.z)
                    break
                case 'S':
                    door.rotateY(THREE.MathUtils.degToRad(90))
                    door.position.set(position.x, position.y, position.z + 0.5)
            }
            this.scale.set(scale.x, scale.y, scale.z)
            parameters.maze.doors.push(door)
            this.doors.push(door)
            this.add(door)
        })
    }
}
