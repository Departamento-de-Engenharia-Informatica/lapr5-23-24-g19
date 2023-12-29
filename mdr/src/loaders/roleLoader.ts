import Container from 'typedi'
import config from '../../config'
import IRoleService from '../services/IServices/IRoleService'

import { Logger } from 'winston'

import { difference as subtract } from 'lodash'
import { constantCase, pascalCase, spaceCase } from 'case-anything'

const domainName = (str: string) => spaceCase(pascalCase(str))

export default async (roles: string[]) => {
    const logger = Container.get('logger') as Logger
    const svc = Container.get(config.services.role.name) as IRoleService

    const activeRoles = (await svc.getRoles())
        .getOrThrow()
        .map((r) => constantCase(r.name))

    const toAdd = subtract(roles, activeRoles)
    const toDisable = subtract(activeRoles, roles)

    if (toAdd.length > 0) {
        logger.info(`About to add the following roles: ${toAdd}`)
        toAdd.forEach(async (r) => {
            const rDomain = domainName(r)

            const result = await svc.createRole({ name: rDomain })

            if (result.isFailure) {
                logger.error(
                    `Failed to create role: ${r}. Reason: ${result.errorValue()}`,
                )
            } else {
                logger.info(`Created role: ${r}`)
            }
        })
    }

    if (toDisable.length > 0) {
        logger.info(`About to disable the following roles: ${toDisable}`)
        toDisable.forEach(async (r) => {
            const rDomain = domainName(r)

            const result = await svc.updateRole({ name: rDomain, active: false })

            if (result.isFailure) {
                logger.error(
                    `Failed to disable role: ${r}. Reason: ${result.errorValue()}`,
                )
            } else {
                logger.info(`Disabled role: ${r}`)
            }
        })
    }
}
