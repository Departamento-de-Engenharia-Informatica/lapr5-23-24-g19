import { assert } from 'chai'
import { describe, it } from 'mocha'
import { RoomName } from './roomName'

describe('Room Name', () => {
    it('should create a valid name', () => {
        const validName = 'Meeting Room'
        const roomName = RoomName.create(validName)

        assert.isOk(roomName.isSuccess)
    })

    it('should fail when the name is too long', () => {
        const longName = 'A'.repeat(51) // Exceeds the maximum length of 50 characters
        const roomName = RoomName.create(longName)

        assert.isNotOk(roomName.isSuccess)
    })
})
