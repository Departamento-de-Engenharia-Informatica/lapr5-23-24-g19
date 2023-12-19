import { assert } from 'chai'
import { FloorMapContent } from './floorMap'
import { Coordinates } from './Coordinates'
import { MaxFloorDimensions } from '../building/maxFloorDimensions'

describe('FloorMap', () => {
    it('should create valid floor map content', () => {
        const dimensions = MaxFloorDimensions.create(10, 10).getValue()
        const mapContent = [
            [0, 1, 0],
            [1, 0, 1],
            [0, 1, 0],
        ]
        const passages = [Coordinates.create(1, 1).getValue()]
        const rooms = [
            Coordinates.create(0, 0).getValue(),
            Coordinates.create(2, 2).getValue(),
        ]
        const elevators = [Coordinates.create(1, 0).getValue()]

        const props = {
            path: '',
            map: {
                dimensions,
                // mapContent,
                // passages,
                // rooms,
                // elevators,
            },
        }

        const result = FloorMapContent.create(props)

        assert.isOk(result.isSuccess)
        const floorMapContent = result.getValue()
        assert.deepEqual(floorMapContent.dimensions, dimensions)
        // assert.deepEqual(floorMapContent.mapContent, mapContent);
        // assert.deepEqual(floorMapContent.passages, passages);
        // assert.deepEqual(floorMapContent.rooms, rooms);
        // assert.deepEqual(floorMapContent.elevators, elevators);
    })

    it('should fail when creating floor map content with null or undefined dimensions', () => {
        const mapContent = [
            [0, 1, 0],
            [1, 0, 1],
            [0, 1, 0],
        ]
        const passages = [Coordinates.create(1, 1).getValue()]
        const rooms = [
            Coordinates.create(0, 0).getValue(),
            Coordinates.create(2, 2).getValue(),
        ]
        const elevators = [Coordinates.create(1, 0).getValue()]

        const props = {
            path: '',
            map: {
                dimensions: null,
                // mapContent,
                // passages,
                // rooms,
                // elevators,
            },
        }

        const result = FloorMapContent.create(props)
        assert.isNotOk(result.isSuccess)
    })
})
