import { assert } from 'chai'
import { describe, it } from 'mocha'
import { RobotTypeCode } from './robotTypeCode'

describe('RobotTypeCode', () => {
    it('should create a valid robot type code', () => {
        const validCodeValue = 'RBT123'
        const robotTypeCode = RobotTypeCode.create(validCodeValue)

        assert.isOk(robotTypeCode.isSuccess)
    })

    it('should fail when creating a robot type code with null or undefined value', () => {
        const robotTypeCode1 = RobotTypeCode.create(null)
        assert.isNotOk(robotTypeCode1.isSuccess)

        const robotTypeCode2 = RobotTypeCode.create(undefined)
        assert.isNotOk(robotTypeCode2.isSuccess)
    })

    it('should fail when the code does not match the regex pattern', () => {
        const invalidCode = 'Invalid Code!' // Contains special characters
        const robotTypeCode = RobotTypeCode.create(invalidCode)

        assert.isNotOk(robotTypeCode.isSuccess)
    })

    it('should fail when the code exceeds the maximum length', () => {
        const longCode = 'A'.repeat(26) // Exceeds the maximum length of 25 characters
        const robotTypeCode = RobotTypeCode.create(longCode)

        assert.isNotOk(robotTypeCode.isSuccess)
    })

    it('should trim leading and trailing whitespace from the value', () => {
        const codeValue = '   RBT123   '
        const robotTypeCode = RobotTypeCode.create(codeValue)

        assert.isOk(robotTypeCode.isSuccess)
        assert.equal(robotTypeCode.getValue().value, 'RBT123')
    })
})
