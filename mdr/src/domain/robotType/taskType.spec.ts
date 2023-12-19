import { assert } from 'chai'
import { describe, it } from 'mocha'
import { TaskType } from './taskType'

describe('TaskType', () => {
    it('should create surveillence task type, unsensitive case', () => {
        const validTaskTypeValue = 'SurVeIllance'
        const taskType = TaskType.toType(validTaskTypeValue)

        assert.equal(taskType, TaskType.SURVEILLANCE)
    })

    it('should create surveillence task type', () => {
        const validTaskTypeValue = 'Surveillance'
        const taskType = TaskType.toType(validTaskTypeValue)

        assert.equal(taskType, TaskType.SURVEILLANCE)
    })

    it('should trim leading and trailing whitespace from the value', () => {
        const taskTypeValue = '   SURVEILLANCE     '
        const taskType = TaskType.toType(taskTypeValue)

        assert.equal(taskType, TaskType.SURVEILLANCE)
    })
    it('should parse task type to string', () => {
        const taskTypeValue = 'SURVEILLANCE'
        const taskType = TaskType.toString(TaskType.SURVEILLANCE)

        assert.equal(taskType, taskTypeValue.trim().toUpperCase())
    })
    it('should create surveillence task type, unsensitive case', () => {
        const validTaskTypeValue = 'DelivERY'
        const taskType = TaskType.toType(validTaskTypeValue)

        assert.equal(taskType, TaskType.DELIVERY)
    })
    it('should create surveillence task type', () => {
        const validTaskTypeValue = 'Delivery'
        const taskType = TaskType.toType(validTaskTypeValue)

        assert.equal(taskType, TaskType.DELIVERY)
    })

    it('should trim leading and trailing whitespace from the value', () => {
        const taskTypeValue = '   DELIVERY     '
        const taskType = TaskType.toType(taskTypeValue)

        assert.equal(taskType, TaskType.DELIVERY)
    })

    it('should parse task type to string', () => {
        const taskTypeValue = 'DELIVERY'
        const taskType = TaskType.toString(TaskType.DELIVERY)

        assert.equal(taskType, taskTypeValue.trim().toUpperCase())
    })
})
