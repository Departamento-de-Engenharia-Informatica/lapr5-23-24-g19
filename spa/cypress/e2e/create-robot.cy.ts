function loginViaAuth0Ui(username: string, password: string) {
    cy.origin(
        Cypress.env('auth_domain'),
        { args: { username, password } },
        ({ username, password }) => {
            cy.get('input#1-email').type(username)
            cy.get('input#1-password').type(password, { log: false })
            cy.get('button[type="submit"]')
                .should('be.visible')
                .should('contain.text', 'Log In')
                .should('not.be.disabled')
                .should('not.be.hidden')
                .click()
        },
    )
}

describe('Robot: createRobot() e2e tests', () => {
    beforeEach(() => {
        cy.intercept('GET', `http://localhost:4000/api/robottypes`, {
            body: [
                {
                    code: 'R02',
                    brand: 'RobotTypeBrandLapr',
                    model: 'ModelLapr',
                    taskTypes: ['SURVEILLANCE'],
                },
                {
                    code: 'R06',
                    brand: 'RobotTypeBrandLapr',
                    model: 'ModelLapr',
                    taskTypes: ['SURVEILLANCE'],
                },
            ],
        }).as('getRobotTypes')

        cy.intercept('GET', 'http://localhost:4000/api/robots', {
            body: [
                {
                    code: 'MVI24',
                    nickname: 'george',
                    state: 0,
                    serialNumber: 'MDNM129AJ133',
                    description: 'Lorem ipsum dolor sit amet',
                    typeCode: 'R02',
                },
                {
                    code: 'MVI254',
                    nickname: 'fafa',
                    state: 1,
                    serialNumber: 'MDNM129AJ533',
                    description: 'Lorem ipsum dolor sit amet',
                    typeCode: 'R02',
                },
            ],
        }).as('getRobots')
        window.localStorage.setItem('USER_ROLES', 'ADM')

        cy.visit('fleet/robots/create')
        const log = Cypress.log({
            displayName: 'AUTH0 LOGIN',
            message: [`🔐 Authenticating | ${Cypress.env('auth_username')}`],
            // @ts-ignore
            autoEnd: false,
        })
        log.snapshot('before')

        loginViaAuth0Ui(Cypress.env('auth_username'), Cypress.env('auth_password'))

        log.snapshot('after')
        log.end()
    })

    it('should have the correct title', () => {
        cy.title().should('equal', 'Create Robot')
    })

    it('should display the form elements', () => {
        cy.wait('@getRobotTypes')
        cy.wait('@getRobots')

        cy.get('#selectedRobotType').should('exist')
        cy.get('#code').should('exist')
        cy.get('#nickname').should('exist')
        cy.get('#serialNumber').should('exist')
        cy.get('#description').should('exist')
    })

    it('should display a list of robot types in the select dropdown', () => {
        cy.wait('@getRobotTypes')
        cy.wait('@getRobots')

        cy.get('#selectedRobotType').select('R02').should('have.value', 'R02')
        cy.get('#selectedRobotType').select('R06').should('have.value', 'R06')
    })

    it('should display existing robots', () => {
        cy.wait('@getRobotTypes')
        cy.wait('@getRobots')

        cy.get('.floor-card').should('have.length', 2)
        cy.get('.floor-card').should('contain.text', 'MVI24')
        cy.get('.floor-card').should('contain.text', 'MVI254')
    })

    it('should be able to create a new robot', () => {
        cy.wait('@getRobotTypes')
        cy.wait('@getRobots')

        const code = 'TVI24'
        const typeCode = 'R02'
        const nickname = 'MarcoMaia'
        const serialNumber = 'MKNM929AJ578'
        const description = 'New Floor'

        cy.get('#selectedRobotType').select(typeCode)
        cy.get('#code').type(code)
        cy.get('#nickname').type(nickname)
        cy.get('#serialNumber').type(serialNumber)
        cy.get('#description').type(description)

        cy.intercept('POST', 'http://localhost:4000/api/robots', {
            statusCode: 201,
            body: {
                code: code,
                nickname: nickname,
                state: 1,
                serialNumber: serialNumber,
                description: description,
                typeCode: typeCode,
            },
        }).as('createRobot')

        cy.intercept('GET', 'http://localhost:4000/api/robots', {
            body: [
                {
                    code: code,
                    nickname: nickname,
                    state: 1,
                    serialNumber: serialNumber,
                    description: description,
                    typeCode: typeCode,
                },
                {
                    code: 'MVI24',
                    nickname: 'george',
                    state: 0,
                    serialNumber: 'MDNM129AJ133',
                    description: 'Lorem ipsum dolor sit amet',
                    typeCode: 'R02',
                },
                {
                    code: 'MVI254',
                    nickname: 'fafa',
                    state: 1,
                    serialNumber: 'MDNM129AJ533',
                    description: 'Lorem ipsum dolor sit amet',
                    typeCode: 'R02',
                },
            ],
        }).as('getRobots')

        cy.get('button[type="submit"]').click()
        cy.wait('@createRobot')
        cy.wait('@getRobots')

        cy.get('.floor-card').should('exist')
        cy.get('.floor-card').should('contain.text', code)
    })

    it('should handle form validation', () => {
        cy.wait('@getRobotTypes')
        cy.wait('@getRobots')

        const code = 'TVI24'
        const typeCode = 'R02'
        const nickname = 'MarcoMaia'
        const serialNumber = 'MKNM929AJ578'
        const description = 'New Floor'

        cy.get('button[type="submit"]').should('be.disabled')

        cy.get('#selectedRobotType').should('have.class', 'ng-invalid')
        cy.get('#code').should('have.class', 'ng-invalid')
        cy.get('#nickname').should('have.class', 'ng-invalid')
        cy.get('#serialNumber').should('have.class', 'ng-invalid')

        cy.get('#selectedRobotType').select(typeCode)
        cy.get('button[type="submit"]').should('be.disabled')

        cy.get('#description').type(
            'This is optional but the form should still be disabled without typing the other properties',
        )
        cy.get('button[type="submit"]').should('be.disabled')

        cy.get('#code').type(code)
        cy.get('#nickname').type(nickname)
        cy.get('#serialNumber').type(serialNumber)
        cy.get('button[type="submit"]').should('not.be.disabled')
    })

    it('should reset the form after floor creation', () => {
        cy.wait('@getRobotTypes')
        cy.wait('@getRobots')

        const code = 'TVI24'
        const typeCode = 'R02'
        const nickname = 'MarcoMaia'
        const serialNumber = 'MKNM929AJ578'
        const description = 'New Floor'

        cy.get('#selectedRobotType').select(typeCode)
        cy.get('#code').type(code)
        cy.get('#nickname').type(nickname)
        cy.get('#serialNumber').type(serialNumber)
        cy.get('#description').type(description)

        cy.intercept('POST', 'http://localhost:4000/api/robots', {
            statusCode: 201,
            body: {
                code: code,
                nickname: nickname,
                state: 1,
                serialNumber: serialNumber,
                description: description,
                typeCode: typeCode,
            },
        }).as('createRobot')

        cy.intercept('GET', 'http://localhost:4000/api/robots', {
            body: [
                {
                    code: code,
                    nickname: nickname,
                    state: 1,
                    serialNumber: serialNumber,
                    description: description,
                    typeCode: typeCode,
                },
                {
                    code: 'MVI24',
                    nickname: 'george',
                    state: 0,
                    serialNumber: 'MDNM129AJ133',
                    description: 'Lorem ipsum dolor sit amet',
                    typeCode: 'R02',
                },
                {
                    code: 'MVI254',
                    nickname: 'fafa',
                    state: 1,
                    serialNumber: 'MDNM129AJ533',
                    description: 'Lorem ipsum dolor sit amet',
                    typeCode: 'R02',
                },
            ],
        }).as('getRobots')

        cy.get('button[type="submit"]').click()
        cy.wait('@createRobot')
        cy.wait('@getRobots')

        cy.get('#code').should('have.value', '')
        cy.get('#nickname').should('have.value', '')
        cy.get('#serialNumber').should('have.value', '')
        cy.get('#description').should('have.value', '')
    })
})
