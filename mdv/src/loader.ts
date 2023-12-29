import * as THREE from 'three'

export interface Loader {
    load<T>(url: string): Promise<T>
}

export class NodeLoader implements Loader {
    private cache: { [url in string]: unknown } = {}
    private tokena!: string

    async load<T>(url: string): Promise<T> {
        if (!!this.cache[url]) {
            return Promise.resolve(this.cache[url] as T)
        }
        // if(this.tokena==undefined){
        //     this.token()
        // }

        const requestOptions: RequestInit = {
            headers: {
                'Authorization': `Bearer BACKEND`,
            }
        };

        return await fetch(url,requestOptions)
            .then(async (res) => {
                const json = await res.json()

                this.cache[url] = json
                return json
            })
            .catch((err) => {
                const errmsg = `Error fetching @ ${url}: ${err}`
                console.error(errmsg)
                return Promise.reject(errmsg)
            })
    }
    async token(): Promise<string> {
        const tokenUrl = 'https://dev-wt48psyid1ra2e8l.us.auth0.com/oauth/token';
        const requestOptions: RequestInit = {
            method: 'POST',
            headers: {
                'Content-Type': 'application/json'
            },
            body:'{"client_id":"9DL31vQCAspYirMRZQqBOsfFeP8sxKZF","client_secret":"u56MpSoADM9AkD3ZGD8AC7p59-sPANDBfy9hTM_ROa_Qo82K3upcXRZKMesopThc","audience":"https://dev-wt48psyid1ra2e8l.us.auth0.com/api/v2/","grant_type":"client_credentials"}'

        };
        return fetch(tokenUrl,requestOptions)
            .then(response => response.json())
            .then(data => {
                // Assuming the token is in a field named 'token' in the JSON response
                this.tokena = data.token;
                console.log(this.tokena)
                return data.token;
            })
            .catch(err => {
                console.error(`Error fetching token: ${err}`);
                return Promise.reject(`Error fetching token: ${err}`);
            });
    }
}

// '{"client_id":"3kYv5HRAErcdGhxZzZFfosQTptNAjuFo","client_secret":"62eHMW3kcJjlRBbssjSeTlLJJX1RqtlLLfbetvazOfCfNSuZoBlzresbXdAdeQDd","audience":"https://dev-wt48psyid1ra2e8l.us.auth0.com/api/v2/","grant_type":"client_credentials"}' };
export class ThreeLoader implements Loader {
    private loader: THREE.FileLoader

    constructor() {
        THREE.Cache.enabled = true
        this.loader = new THREE.FileLoader().setResponseType('json')
    }

    async load<T>(url: string): Promise<T> {
        return this.loader.loadAsync(url) as T
    }
}
