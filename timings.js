
function timings(arr)
{
    return Promise.all(arr.map(promise_wait))
}

function promise_wait(n)
{
    return new Promise((resolve, reject) =>
		       {
			   setTimeout(() =>
				      {
					  console.log(n)
					  resolve(n)
				      }, n * 1000)
		       })
}
