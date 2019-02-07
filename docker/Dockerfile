FROM owl
USER opam

RUN sudo apt-get -y install libzmq3-dev netcat iproute2 dnsutils liblapacke
RUN opam install cstruct-lwt lwt_ppx mirage-time-lwt zmq-lwt
ADD . /code
WORKDIR /code
RUN sudo chown -R opam:opam .
ENTRYPOINT ["bash", "/code/run.sh"]
CMD ["server"]
